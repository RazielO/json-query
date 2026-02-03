module Main (main) where

import JsonParserTest (jsonParserTests)
import ParserTest (parserTests)
import QueryEvalTest (queryEvalTests)
import QueryParserTest (queryParserTests)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  parserTests
  jsonParserTests
  queryParserTests
  queryEvalTests