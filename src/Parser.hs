{-# LANGUAGE InstanceSigs #-}

module Parser
  ( Parser (..),
    parseChar,
    parseString,
    parseSpan,
    many,
    some,
    optional,
    notFollowedBy,
    satisfy,
    sepBy,
    sepBy1,
    lexeme,
    parseWhitespace,
    symbol,
    between,
    eof
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (void)
import Text.Printf (printf)

-- | Generic parser type
newtype Parser a = Parser
  { runParser ::
      -- String to parse
      String ->
      -- Line number
      Int ->
      -- Column number
      Int ->
      -- Result of the parse
      Either
        -- Error (line, char, error message)
        (Int, Int, String)
        -- Ok (Rest of the string, new line, new column, parsed result)
        (String, Int, Int, a)
  }

-- Functor of parser
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser runParser') =
    Parser $ \input line char -> do
      (input', line', column', rest) <- runParser' input line char
      Right (input', line', column', f rest)

-- Application of parser's functor
instance Applicative Parser where
  -- Embed pure results
  pure :: a -> Parser a
  pure value = Parser (\input line column -> Right (input, line, column, value))

  -- Sequence computations and combine results
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser p1) <*> (Parser p2) = Parser $ \input line column -> do
    (input', line', column', f) <- p1 input line column
    (input'', line'', column'', a) <- p2 input' line' column'
    Right (input'', line'', column'', f a)

-- Helper to measure how far a parser progressed (by comparing columns).
-- If both errors are at same line, prefer larger column; otherwise prefer larger line.
preferError :: (Int, Int, String) -> (Int, Int, String) -> (Int, Int, String)
preferError error1@(line1, col1, _) error2@(line2, col2, _)
  | line1 > line2 = error1
  | line2 > line1 = error2
  | col1 >= col2 = error1
  | otherwise = error2

-- Monoid that selects the non-empty value
instance Alternative Parser where
  empty :: Parser a
  empty = Parser (\_ line column -> Left (line, column, ""))

  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) = Parser $ \str line column ->
    case p1 str line column of
      Right ok -> Right ok
      Left err1 -> case p2 str line column of
        Right ok -> Right ok
        Left err2 -> Left (preferError err1 err2)

-- | Monad instance
instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser p >>= f = Parser $ \input line col ->
    case p input line col of
      Left err -> Left err
      Right (input', line', column', rest) -> runParser (f rest) input' line' column'

-- | Try to parse a character
parseChar ::
  -- | Character to try to parse
  Char ->
  -- | Parser to parse the character
  Parser Char
parseChar chr = Parser runParser'
  where
    runParser' (chr' : rest) line column
      | chr == chr' = Right (rest, line, column + 1, chr)
      | otherwise = Left (line, column, printf "Expected '%c' but got '%c' on line %d:%d." chr chr' line column)
    runParser' [] line column = Left (line, column, printf "Expected '%c' but got empty string on line %d:%d." chr line column)

-- | Try to parse a string
parseString ::
  -- | String to try to parse
  String ->
  -- | Parser to parse the string
  Parser String
parseString string = Parser runParser'
  where
    runParser' input line column =
      let length' = length string
       in case splitAt length' input of
            (prefix, rest)
              | string == prefix -> Right (rest, line, column + length', string)
              | null prefix -> Left (line, column, printf "Expected \"%s\" but got empty string on line %d:%d" string line column)
              | otherwise -> Left (line, column, printf "Expected \"%s\" but got \"%s\" on line %d:%d" string prefix line column)

-- | Parse a span of elements that match a predicate
parseSpan ::
  -- | Function that evaluates if a char should be included
  (Char -> Bool) ->
  -- | Parser of the span
  Parser String
parseSpan predicate =
  Parser $ \input line column ->
    let (token, rest) = span predicate input
        (line', column') = foldl update (line, column) token
     in Right (rest, line', column', token)
  where
    update :: (Int, Int) -> Char -> (Int, Int)
    update (line', column') chr = case chr of
      '\n' -> (line' + 1, column')
      _ -> (line', column' + 1)

-- | Parses zero or more elements that successfully executes the parser
many :: Parser a -> Parser [a]
many parser = some parser <|> pure []

-- | Parses one or more elements that successfully executes the parser
some :: Parser a -> Parser [a]
some parser = (:) <$> parser <*> many parser

-- | Try to parse a value, if it can be parsed, consume it in a Just, otherwise, do not consume the input
optional :: Parser a -> Parser (Maybe a)
optional parse = (Just <$> parse) <|> pure Nothing

-- | If the parse is successful, it fails, used to exclude invalid elements
notFollowedBy :: Parser a -> Parser ()
notFollowedBy (Parser runParser') = Parser $ \input line col ->
  case runParser' input line col of
    Right _ -> Left (line, col, "Unexpected value")
    Left _ -> Right (input, line, col, ())

-- | Parse a character that matches the predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \input line column ->
  case input of
    (chr : cs)
      | predicate chr ->
          let (line', column') = case chr of
                '\n' -> (line + 1, 1)
                _ -> (line, column + 1)
           in Right (cs, line', column', chr)
    (chr : _) -> Left (line, column, printf "Unexpected '%c'" chr)
    [] -> Left (line, column, "Unexpected end of input")

-- | Parse one or more occurrences of `parser` separated by `separator`
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 parser separator = (:) <$> parser <*> many (separator *> parser)

-- | Parse zero or more occurrences of `parser` separated by `separator`
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy parser separator = sepBy1 parser separator <|> pure []

-- | Parse an element that is enclosed in two other parsers' elements
between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

-- | Parse a whitespace (space, tab, new line, carriage return)
parseWhitespace :: Parser ()
parseWhitespace = void (many (satisfy (`elem` (" \t\n\r" :: [Char]))))

-- | Execute a parser that can end with whitespaces
lexeme :: Parser a -> Parser a
lexeme parser = parser <* parseWhitespace

-- | Parse a string that can end with whitespaces
symbol :: String -> Parser String
symbol = lexeme . parseString

-- | Ensure there is no more input (EOF)
eof :: Parser ()
eof = Parser $ \input line col ->
  case input of
    [] -> Right ([], line, col, ())
    _  -> Left (line, col, "Expected end of input")
