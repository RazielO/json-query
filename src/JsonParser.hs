module JsonParser (JsonValue (..), jsonBool, jsonNull, jsonNumber, jsonArray, jsonString, jsonObject, json) where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (replicateM)
import Data.Char (chr, isDigit, isHexDigit, ord)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList)
import Data.Maybe (fromMaybe)
import Parser (Parser (..), between, eof, lexeme, many, notFollowedBy, optional, parseChar, parseWhitespace, satisfy, sepBy, some, symbol)

-- | Top-level JSON elements
data JsonValue
  = JsonObject (Map String JsonValue)
  | JsonArray [JsonValue]
  | JsonString String
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
    parsePair :: Parser (String, JsonValue)
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
    parseBool :: String -> Bool -> Parser JsonValue
    parseBool str value = JsonBool value <$ symbol str

    boolTrue = parseBool "true" True
    boolFalse = parseBool "false" False

-- | Parser for a number value
jsonNumber :: Parser JsonValue
jsonNumber = (JsonDouble <$> parseDouble) <|> (JsonInteger <$> parseInteger)
  where
    -- Parse a double number
    parseDouble :: Parser Double
    parseDouble =
      read <$> do
        integer <- parseInteger'
        decimals <- ('.' :) <$> (parseChar '.' *> some digit)
        expn' <- optional fracExponent
        let expn = fromMaybe "" expn'
        pure (integer ++ decimals ++ expn)
      where
        fracExponent :: Parser String
        fracExponent = do
          e <- parseChar 'e' <|> parseChar 'E'
          sign' <- optional (parseChar '-' <|> parseChar '+')
          let sign = fromMaybe '+' sign'
          ds <- some digit
          pure (e : sign : ds)

    -- Parse an integer number
    parseInteger :: Parser Integer
    parseInteger = read <$> parseInteger'

    -- Parse an integer as a string
    parseInteger' :: Parser String
    parseInteger' = do
      sign <- optional (parseChar '-')
      numbers <- parseZero <|> parseNonZero
      pure $ maybe numbers (const ('-' : numbers)) sign
      where
        parseZero :: Parser String
        parseZero = do
          _ <- parseChar '0'
          notFollowedBy digit
          pure "0"

        parseNonZero :: Parser String
        parseNonZero =
          (:)
            <$> satisfy (\c -> isDigit c && c /= '0')
            <*> many digit

    -- Parse a digit
    digit :: Parser Char
    digit = satisfy isDigit

-- | Parser for a json string
jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

-- | Parser for a json string literal
stringLiteral :: Parser String
stringLiteral = between (parseChar '"') (parseChar '"') (many (parseEscapeHex <|> parseEscapeCharacter <|> satisfy validChar))
  where
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
