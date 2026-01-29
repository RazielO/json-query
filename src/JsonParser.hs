{-# LANGUAGE OverloadedStrings #-}

module JsonParser
  ( JsonValue (..),
    jsonBool,
    jsonNull,
    jsonNumber,
    jsonArray,
    jsonString,
    jsonObject,
    json,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (replicateM)
import Data.Char (chr, isDigit, isHexDigit, ord)
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text (cons, pack, unpack)
import qualified Data.Text.Lazy as TextL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder (singleton, toLazyText)
import JsonValue (JsonValue (..))
import Parser (Lookahead (..), Parser (..), between, eof, failParser, lexeme, many, notFollowedBy, optional, parseChar, parseWhitespace, predict, satisfy, sepBy, symbol)

-- | Parser for a json
json :: Parser JsonValue
json = parseWhitespace *> jsonValue <* eof

-- | Parser for a json value
jsonValue :: Parser JsonValue
jsonValue =
  lexeme $
    predict
      [ (LAChar '{', jsonObject),
        (LAChar '[', jsonArray),
        (LAChar '"', jsonString),
        (LAChar 't', jsonBool),
        (LAChar 'f', jsonBool),
        (LAChar 'n', jsonNull),
        (LAChar '-', jsonNumber),
        (LADigit, jsonNumber)
      ]
      ["object", "array", "string", "number", "true", "false", "null"]

-- | Parser for a json object
jsonObject :: Parser JsonValue
jsonObject = do
  _ <- symbol "{"
  pairs <- sepBy parsePair (symbol ",")
  _ <- symbol "}"
  case findDuplicateKey pairs of
    Just k -> failParser ("Duplicate key: \"" <> Text.unpack k <> "\"")
    Nothing -> pure (Object pairs)
  where
    parsePair :: Parser (Text, JsonValue)
    parsePair = do
      key <- lexeme stringLiteral
      _ <- symbol ":"
      val <- jsonValue
      pure (key, val)

    findDuplicateKey :: [(Text, JsonValue)] -> Maybe Text
    findDuplicateKey pairs = go pairs Set.empty
      where
        go :: [(Text, JsonValue)] -> Set Text -> Maybe Text
        go [] _ = Nothing
        go ((k, _) : xs) seen
          | Set.member k seen = Just k
          | otherwise = go xs (Set.insert k seen)

-- | Parser for a json array
jsonArray :: Parser JsonValue
jsonArray = do
  _ <- symbol "["
  values <- sepBy jsonValue (symbol ",")
  _ <- symbol "]"
  pure (Array values)

-- | Parser for a null value
jsonNull :: Parser JsonValue
jsonNull = Null <$ symbol "null"

-- | Parser for a boolean value
jsonBool :: Parser JsonValue
jsonBool = boolTrue <|> boolFalse
  where
    boolTrue :: Parser JsonValue
    boolTrue = Boolean True <$ symbol "true"

    boolFalse :: Parser JsonValue
    boolFalse = Boolean False <$ symbol "false"

-- | Parser for a number value
jsonNumber :: Parser JsonValue
jsonNumber = (Number <$> parseDouble) <|> (Number <$> parseInteger)
  where
    -- Parse a double number
    parseDouble :: Parser Scientific
    parseDouble = read . Text.unpack <$> parseDoubleText
      where
        parseDoubleText :: Parser Text
        parseDoubleText = do
          integer <- parseInteger'
          decimals <- Text.cons '.' <$> (parseChar '.' *> digits)
          expn' <- optional fracExponent
          let expn = fromMaybe "" expn'
          pure (integer <> decimals <> expn)

        fracExponent :: Parser Text
        fracExponent = do
          e <- parseChar 'e' <|> parseChar 'E'
          sign <- fromMaybe '+' <$> optional (parseChar '-' <|> parseChar '+')
          ds <- digits
          pure (Text.pack [e, sign] <> ds)

    -- Parse an integer number
    parseInteger :: Parser Scientific
    parseInteger = read . Text.unpack <$> parseInteger'

    -- Parse an integer as a string
    parseInteger' :: Parser Text
    parseInteger' = do
      sign <- optional (parseChar '-')
      numbers <- parseZero <|> parseNonZero
      pure $ case sign of
        Nothing -> numbers
        Just _ -> Text.cons '-' numbers
      where
        parseZero :: Parser Text
        parseZero = do
          _ <- parseChar '0'
          notFollowedBy digit
          pure "0"

        parseNonZero :: Parser Text
        parseNonZero =
          Text.cons
            <$> satisfy (\c -> isDigit c && c /= '0')
            <*> digits

    -- Parse digits as Text
    digits :: Parser Text
    digits = Text.pack <$> many digit

    -- Parse a digit
    digit :: Parser Char
    digit = satisfy isDigit

-- | Parser for a json string
jsonString :: Parser JsonValue
jsonString = Str <$> stringLiteral

-- | Parser for a json string literal
stringLiteral :: Parser Text
stringLiteral =
  TextL.toStrict . Builder.toLazyText . mconcat
    <$> between
      (parseChar '"')
      (parseChar '"')
      (many $ charBuilder <$> (parseEscapeHex <|> parseEscapeCharacter <|> satisfy validChar))
  where
    charBuilder :: Char -> Builder
    charBuilder = Builder.singleton

    -- Validate a character
    validChar :: Char -> Bool
    validChar chr' =
      let code = ord chr'
       in code >= 0x0020 && code <= 0x10FFF && chr' /= '"' && chr' /= '\\'

    -- Parse escape values
    parseEscapeCharacter :: Parser Char
    parseEscapeCharacter =
      parseChar '\\'
        *> ( '"' <$ parseChar '"'
               <|> '\\' <$ parseChar '\\'
               <|> '/' <$ parseChar '/'
               <|> '\b' <$ parseChar 'b'
               <|> '\f' <$ parseChar 'f'
               <|> '\n' <$ parseChar 'n'
               <|> '\r' <$ parseChar 'r'
               <|> '\t' <$ parseChar 't'
           )

    -- Parse hex value
    parseEscapeHex :: Parser Char
    parseEscapeHex = do
      _ <- parseChar '\\'
      _ <- parseChar 'u'
      digits <- replicateM 4 (satisfy isHexDigit)
      let code = read ("0x" ++ digits)
      pure (chr code)
