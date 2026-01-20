{-# LANGUAGE OverloadedStrings #-}

module JsonParser (JsonValue (..), jsonBool, jsonNull, jsonNumber, jsonArray, jsonString, jsonObject, json) where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (replicateM)
import Data.Char (chr, isDigit, isHexDigit, ord)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text (cons, pack, unpack)
import qualified Data.Text.Lazy as TextL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder (singleton, toLazyText)
import Parser (Parser (..), between, eof, lexeme, many, notFollowedBy, optional, parseChar, parseWhitespace, satisfy, sepBy, symbol)

-- | Top-level JSON elements
data JsonValue
  = JsonObject (Map Text JsonValue)
  | JsonArray [JsonValue]
  | JsonString Text
  | JsonInteger Integer
  | JsonDouble Double
  | JsonBool Bool
  | JsonNull
  deriving (Show, Eq)

-- | Parser for a json
json :: Parser JsonValue
json = parseWhitespace *> jsonValue <* eof

-- | Parser for a json value
jsonValue :: Parser JsonValue
jsonValue = lexeme $ jsonObject <|> jsonArray <|> jsonNumber <|> jsonBool <|> jsonNull <|> jsonString

-- | Parser for a json object
jsonObject :: Parser JsonValue
jsonObject = between (symbol "{") (symbol "}") (JsonObject . Map.fromList <$> sepBy parsePair (symbol ","))
  where
    parsePair :: Parser (Text, JsonValue)
    parsePair = (\key _ value -> (key, value)) <$> lexeme stringLiteral <*> symbol ":" <*> jsonValue

-- | Parser for a json array
jsonArray :: Parser JsonValue
jsonArray = between (symbol "[") (symbol "]") arrayElements
  where
    -- Parse comma-separated json values
    arrayElements :: Parser JsonValue
    arrayElements = JsonArray <$> sepBy jsonValue (symbol ",")

-- | Parser for a null value
jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ symbol "null"

-- | Parser for a boolean value
jsonBool :: Parser JsonValue
jsonBool = boolTrue <|> boolFalse
  where
    boolTrue :: Parser JsonValue
    boolTrue = JsonBool True <$ symbol "true"

    boolFalse :: Parser JsonValue
    boolFalse = JsonBool False <$ symbol "false"

-- | Parser for a number value
jsonNumber :: Parser JsonValue
jsonNumber = (JsonDouble <$> parseDouble) <|> (JsonInteger <$> parseInteger)
  where
    -- Parse a double number
    parseDouble :: Parser Double
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
    parseInteger :: Parser Integer
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
jsonString = JsonString <$> stringLiteral

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
