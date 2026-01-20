module JsonParser (JsonValue (..), jsonBool, jsonNull, jsonNumber) where

import Control.Applicative (Alternative ((<|>)))
import Data.Char (isDigit)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Parser (Parser (..), many, notFollowedBy, optional, parseChar, parseString, satisfy, some)

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

-- | Parser for a null value
jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ parseString "null"

-- | Parser for a boolean value
jsonBool :: Parser JsonValue
jsonBool = boolTrue <|> boolFalse
  where
    parseBool :: String -> Bool -> Parser JsonValue
    parseBool str value = JsonBool value <$ parseString str

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