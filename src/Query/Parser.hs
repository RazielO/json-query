module Query.Parser
  ( query,
    dotExpr,
    iterator,
    objectIndex,
    arrayIndex,
    pipe,
    comma,
    slice,
    postfixExpr
  )
where

import Common.Parser (stringLiteral)
import Control.Applicative (Alternative ((<|>)))
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text (cons, unpack)
import Parser (Parser, chainl1, char, eof, failParser, lexeme, many, optional, parseWhitespace, satisfy, spanP)
import Query.AST (Query (..))
import Text.Printf (printf)
import Text.Read (readMaybe)

-- | Parser for a query
query :: Parser Query
query = parseWhitespace *> comma <* eof

-- | Operations query parser
dotExpr :: Parser Query
dotExpr = do
  _ <- lexeme (char '.')
  first <- objectIndex <|> bracketOp <|> identity
  rest <- many (objectIndex <|> bracketOp)
  pure (foldl Pipe first rest)

-- | Parser for operations that require brackets
bracketOp :: Parser Query
bracketOp =
  slice
    <|> arrayIndex
    <|> iterator

-- | Parser for a comma query
comma :: Parser Query
comma = chainl1 pipe commaOp
  where
    commaOp :: Parser (Query -> Query -> Query)
    commaOp = Comma <$ (parseWhitespace *> lexeme (char ','))

-- | Parser for a piped query
pipe :: Parser Query
pipe = chainl1 postfixExpr pipeOp
  where
    pipeOp :: Parser (Query -> Query -> Query)
    pipeOp = Pipe <$ (parseWhitespace *> lexeme (char '|'))

-- | Parser for postfix operations that allow chaining
-- postfixExpr :: Parser Query
-- postfixExpr = chainl1 primary (pure Pipe)
postfixExpr :: Parser Query
postfixExpr = do
  first <- primary
  rest  <- many primary
  pure (foldl Pipe first rest)


-- | Parser for primary values (parentheses or identity)
primary :: Parser Query
primary = group <|> dotExpr

-- | Parser for a parentheses-grouped query
group :: Parser Query
group =
  lexeme (char '(')
    *> comma
    <* lexeme (char ')')

-- | Identity parser
identity :: Parser Query
identity = pure Identity

-- | Iterator parser
iterator :: Parser Query
iterator =
  Iterator
    <$ lexeme (char '[')
    <* lexeme (char ']')
    <*> optionalModifier

-- | Parser for a single object key index
objectIndex :: Parser Query
objectIndex = do
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
  _ <- lexeme (char '[')
  index' <- indexParser
  _ <- lexeme (char ']')
  ArrayIndex index' <$> optionalModifier

-- | Parser for an index number
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
  _ <- lexeme (char '[')
  start' <- optional indexParser
  _ <- lexeme (char ':')
  end' <- optional indexParser
  _ <- lexeme (char ']')

  if isJust start' || isJust end'
    then pure (Slice start' end')
    else failParser (printf "Slice requires a start or end index")