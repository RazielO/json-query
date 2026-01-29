module Main where

import qualified Data.Text as Text (pack)
import JsonParser (json)
import JsonValue (prettyDisplay)
import Parser (Parser (runParser))
import Prettyprinter.Render.Terminal (putDoc)

main :: IO ()
main = do
  input <- getContents
  case runParser json (Text.pack input) 1 1 of
    Left (_, _, err) -> putStrLn err
    Right (_, _, _, result) -> putDoc $ prettyDisplay result
