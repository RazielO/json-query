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
  ( charTest,
    manyTest,
    notFollowedByTest,
    optionalTest,
    parseSpanTest,
    parseStringTest,
    satisfyTest,
    sepBy1Test,
    sepByTest,
    someTest,
  )
import QueryEvalTest
  ( evalIdentityTest,
  )
import QueryParserTest
  ( identityParseTest,
  )
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  -- parser tests
  charTest
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
  -- query parser test
  identityParseTest
  -- eval query test
  evalIdentityTest
