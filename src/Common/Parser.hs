{-# LANGUAGE OverloadedStrings #-}

module Common.Parser
  ( stringLiteral,
    number,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (replicateM)
import Data.Bits (Bits (shiftL))
import Data.Char (chr, isDigit, isHexDigit, ord)
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text (concat, cons, pack, unpack)
import qualified Data.Text.Lazy as TextL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder (singleton, toLazyText)
import Parser (Parser, between, char, failParser, many, notFollowedBy, optional, satisfy, string, some)

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
       in code >= 0x0020
            && code <= 0x10FFFF
            && not (isHighSurrogate code || isLowSurrogate code) -- disallow lone surrogates
            && chr' /= '"'
            && chr' /= '\\'

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
      _ <- char '\\' *> char 'u'
      high <- hex4
      if isHighSurrogate high
        then do
          _ <- char '\\' *> char 'u'
          low <- hex4
          if isLowSurrogate low
            then pure (combineSurrogates high low)
            else failParser "Lone low surrogate in \\u escape"
        else
          if isLowSurrogate high
            then failParser "Lone low surrogate in \\u escape"
            else pure (chr high)

    -- Parse a 4-digit hexadecimal number
    hex4 :: Parser Int
    hex4 = read . ("0x" <>) <$> replicateM 4 (satisfy isHexDigit)

    -- Is the char code the high part of a surrogate pair
    isHighSurrogate :: Int -> Bool
    isHighSurrogate x = x >= 0xD800 && x <= 0xDBFF

    -- Is the char code the low part of a surrogate pair
    isLowSurrogate :: Int -> Bool
    isLowSurrogate x = x >= 0xDC00 && x <= 0xDFFF

    -- Combine a surrogate pair into a char
    combineSurrogates :: Int -> Int -> Char
    combineSurrogates high low = chr $ 0x10000 + ((high - 0xD800) `shiftL` 10) + (low - 0xDC00)

-- | Parse a number
number :: Parser Scientific
number = do
  sign <- fromMaybe "" <$> optional (string "-")
  intPart <- parseZero <|> parseNonZero
  fracPart <- fromMaybe "" <$> optional (Text.cons '.' <$> (char '.' *> (Text.pack <$> some digit)))
  expPart <- fromMaybe "" <$> optional fracExponent
  pure . read . Text.unpack $ Text.concat [sign, intPart, fracPart, expPart]
  where
    fracExponent :: Parser Text
    fracExponent = do
      e <- char 'e' <|> char 'E'
      sign <- fromMaybe '+' <$> optional (char '-' <|> char '+')
      ds <- Text.pack <$> some digit
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
