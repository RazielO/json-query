module Main (main) where

import ParserTest
import JsonParserTest
import Test.Hspec

main :: IO ()
main = hspec $ do
  -- parser tests
  parseCharTest
  parseStringTest
  parseSpanTest
  manyTest
  someTest
  optionalTest
  notFollowedByTest
  satisfyTest
  -- json parse test
  jsonNullTest
  jsonBoolTest
  jsonNumberTest