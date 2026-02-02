{-# LANGUAGE OverloadedStrings #-}

module Common.Parser
  ( stringLiteral,
    number,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (replicateM)
import Data.Char (chr, isDigit, isHexDigit, ord)
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text (concat, cons, pack, unpack)
import qualified Data.Text.Lazy as TextL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder (singleton, toLazyText)
import Parser (Parser, between, char, many, notFollowedBy, optional, satisfy, string)

-- | Parser for a json string literal
stringLiteral :: Parser Text
stringLiteral =
  TextL.toStrict . Builder.toLazyText . mconcat
    <$> between
      (char '"')
      (char '"')
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
      char '\\'
        *> ( '"' <$ char '"'
               <|> '\\' <$ char '\\'
               <|> '/' <$ char '/'
               <|> '\b' <$ char 'b'
               <|> '\f' <$ char 'f'
               <|> '\n' <$ char 'n'
               <|> '\r' <$ char 'r'
               <|> '\t' <$ char 't'
           )

    -- Parse hex value
    parseEscapeHex :: Parser Char
    parseEscapeHex = do
      _ <- char '\\'
      _ <- char 'u'
      digits <- replicateM 4 (satisfy isHexDigit)
      let code = read ("0x" ++ digits)
      pure (chr code)

number :: Parser Scientific
number = do
  sign <- fromMaybe "" <$> optional (string "-")
  integerPart' <- parseZero <|> parseNonZero
  let integerPart = Text.concat [sign, integerPart']

  fractionalPart <- optional $ Text.cons '.' <$> (char '.' *> digits)
  case fractionalPart of
    Nothing -> pure (read $ Text.unpack integerPart)
    Just fraction -> do
      exponent' <- fromMaybe "" <$> optional fracExponent
      pure $ read $ Text.unpack (Text.concat [integerPart, fraction, exponent'])
  where
    fracExponent :: Parser Text
    fracExponent = do
      e <- char 'e' <|> char 'E'
      sign <- fromMaybe '+' <$> optional (char '-' <|> char '+')
      ds <- digits
      pure (Text.pack [e, sign] <> ds)

    parseZero :: Parser Text
    parseZero = do
      _ <- char '0'
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
