module Main (main) where

import JsonParserTest
  ( jsonArrayTest,
    jsonBoolTest,
    jsonNullTest,
    jsonNumberTest,
    jsonObjectTest,
    jsonStringTest,
  )
import ParserTest
  ( manyTest,
    notFollowedByTest,
    optionalTest,
    parseCharTest,
    parseSpanTest,
    parseStringTest,
    satisfyTest,
    sepBy1Test,
    sepByTest,
    someTest,
  )
import Test.Hspec (hspec)

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
  sepByTest
  sepBy1Test
  -- json parse test
  jsonNullTest
  jsonBoolTest
  jsonNumberTest
  jsonArrayTest
  jsonStringTest
  jsonObjectTest