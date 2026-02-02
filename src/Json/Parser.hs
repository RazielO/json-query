{-# LANGUAGE OverloadedStrings #-}

module Json.Parser
  ( Json (..),
    jsonBool,
    jsonNull,
    jsonNumber,
    jsonArray,
    jsonString,
    jsonObject,
    json,
  )
where

import Common.Parser (number, stringLiteral)
import Control.Applicative (Alternative ((<|>)))
import qualified Data.HashMap.Strict as Map (fromList)
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import Json.AST (Json (..))
import Parser (Lookahead (..), Parser (..), eof, failParser, lexeme, parseWhitespace, predict, sepBy, symbol)

-- | Parser for a json
json :: Parser Json
json = parseWhitespace *> jsonValue <* eof

-- | Parser for a json value
jsonValue :: Parser Json
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
jsonObject :: Parser Json
jsonObject = do
  _ <- symbol "{"
  pairs <- sepBy parsePair (symbol ",")
  _ <- symbol "}"
  case findDuplicateKey pairs of
    Just k -> failParser ("Duplicate key: \"" <> Text.unpack k <> "\"")
    Nothing -> pure (Object {objectList = pairs, objectMap = Map.fromList pairs})
  where
    parsePair :: Parser (Text, Json)
    parsePair = do
      key <- lexeme stringLiteral
      _ <- symbol ":"
      val <- jsonValue
      pure (key, val)

    findDuplicateKey :: [(Text, Json)] -> Maybe Text
    findDuplicateKey pairs = go pairs Set.empty
      where
        go :: [(Text, Json)] -> HashSet Text -> Maybe Text
        go [] _ = Nothing
        go ((k, _) : xs) seen
          | Set.member k seen = Just k
          | otherwise = go xs (Set.insert k seen)

-- | Parser for a json array
jsonArray :: Parser Json
jsonArray = do
  _ <- symbol "["
  values <- sepBy jsonValue (symbol ",")
  _ <- symbol "]"
  pure (Array values)

-- | Parser for a null value
jsonNull :: Parser Json
jsonNull = Null <$ symbol "null"

-- | Parser for a boolean value
jsonBool :: Parser Json
jsonBool = boolTrue <|> boolFalse
  where
    boolTrue :: Parser Json
    boolTrue = Boolean True <$ symbol "true"

    boolFalse :: Parser Json
    boolFalse = Boolean False <$ symbol "false"

-- | Parser for a number value
jsonNumber :: Parser Json
jsonNumber = Number <$> number

-- | Parser for a json string
jsonString :: Parser Json
jsonString = Str <$> stringLiteral
