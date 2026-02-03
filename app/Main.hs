{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T (pack)
import qualified Data.Text.IO as TIO
import Json.AST (prettyDisplay)
import Json.Parser (json)
import Parser (Parser (runParser))
import Prettyprinter.Render.Terminal (putDoc)
import Query.Eval (evalQuery)
import Query.Parser (query)
import System.Environment (getArgs)
import Text.Printf (printf)
import Query.Parser (postfixExpr)

main :: IO ()
main = do
  print $ runParser postfixExpr ".foo.bar" 1 1
  args <- getArgs
  case args of
    [query'] -> do
      input <- TIO.getContents
      case runParser query (T.pack query') 1 1 of
        Left (_, _, err) -> putStrLn $ printf "Error on query \"%s\". %s" query' err
        Right (_, _, _, query'') -> case runParser json input 1 1 of
          Left (_, _, err) -> putStrLn err
          Right (_, _, _, json') ->
            case evalQuery query'' json' of
              Right result -> mapM_ (putDoc . prettyDisplay) result
              Left err -> putStrLn $ printf "Error while performing query. %s." err
    _ -> putStrLn "Usage: json-query <query>"