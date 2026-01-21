module Main where

import qualified Data.Text as Text (pack)
import JsonParser (json)
import Parser (Parser (runParser))

main :: IO ()
main = do
  input <- getContents
  case runParser json (Text.pack input) 1 1 of
    Left (_, _, err) -> putStrLn err
    Right val -> print val
