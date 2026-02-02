module Query.Parser
  ( query,
    identity,
    iterator,
    objectIndex,
    arrayIndex,
    pipe,
    comma,
    slice,
  )
where

import Common.Parser (stringLiteral)
import Control.Applicative (Alternative ((<|>)))
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text (cons, unpack)
import Parser (Parser, char, eof, failParser, lexeme, optional, parseWhitespace, satisfy, some, spanP)
import Query.AST (Query (..))
import Text.Printf (printf)
import Text.Read (readMaybe)

-- | Parser for a query
query :: Parser Query
query = parseWhitespace *> comma <* eof

-- | Operations query parser
operation :: Parser Query
operation =
  group
    <|> objectIndex
    <|> slice
    <|> arrayIndex
    <|> iterator
    <|> identity

-- | Parser for a piped query
comma :: Parser Query
comma = do
  first <- pipe
  comma' <- optional (lexeme (char ','))
  if isJust comma'
    then Comma first <$> comma
    else pure first

-- | Parser for a piped query
pipe :: Parser Query
pipe = do
  first <- lexeme operation
  pipe' <- optional (lexeme (char '|'))
  if isJust pipe'
    then Pipe first <$> pipe
    else pure first

-- | Parser for a parentheses-grouped query
group :: Parser Query
group =
  lexeme (char '(')
    *> comma
    <* lexeme (char ')')

-- | Convert a list of queries into pipes
queriesToPipes :: [Query] -> Query
queriesToPipes [] = Identity
queriesToPipes [query'] = query'
queriesToPipes (q : qs) = Pipe q (queriesToPipes qs)

-- | Identity parser
identity :: Parser Query
identity = Identity <$ lexeme (char '.')

-- | Iterator parser
iterator :: Parser Query
iterator =
  Iterator
    <$ lexeme (char '.')
    <* lexeme (char '[')
    <* lexeme (char ']')

-- | Parser for a single or combined object key index
objectIndex :: Parser Query
objectIndex = do
  indices <- some (lexeme objectIndex')
  case indices of
    [index'] -> pure index'
    _ -> pure (queriesToPipes indices)

-- | Parser for a single object key index
objectIndex' :: Parser Query
objectIndex' = do
  _ <- char '.'
  lbracket <- optional (char '[')
  if isJust lbracket
    then do
      key' <- stringLiteral
      _ <- char ']'
      ObjectIndex key' <$> optionalModifier
    else do
      key' <- stringLiteral <|> validKey
      ObjectIndex key' <$> optionalModifier

-- | Parser for a valid simple key (alphanumeric and underscores, but not starting with a digit)
validKey :: Parser Text
validKey = do
  start' <- satisfy (\chr -> isAlpha chr || chr == '_')
  rest <- spanP (\chr -> isAlphaNum chr || chr == '_')
  pure (Text.cons start' rest)

-- | Parser for the optional query
optionalModifier :: Parser Bool
optionalModifier = isJust <$> optional (lexeme $ char '?')

-- | Parser for an array index
arrayIndex :: Parser Query
arrayIndex = do
  _ <- char '.'
  _ <- lexeme (char '[')
  index' <- indexParser
  _ <- lexeme (char ']')
  ArrayIndex index' <$> optionalModifier

indexParser :: Parser Int
indexParser = do
  negative <- optional (char '-')
  index' <- spanP isDigit
  let sign = fromMaybe ' ' negative
  case readMaybe (sign : Text.unpack index') of
    Just idx -> pure idx
    Nothing -> failParser (printf "Invalid index %s" index')

-- | Parser for a string/array slice
slice :: Parser Query
slice = do
  _ <- char '.'
  _ <- lexeme (char '[')
  start' <- optional indexParser
  _ <- lexeme (char ':')
  end' <- optional indexParser
  _ <- lexeme (char ']')

  if isJust start' || isJust end'
    then pure (Slice start' end')
    else failParser (printf "Slice requires a start or end index")