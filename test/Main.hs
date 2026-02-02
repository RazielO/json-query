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
    satisfyTest,
    sepBy1Test,
    sepByTest,
    someTest,
    spanPTest,
    stringTest,
  )
import QueryEvalTest
  ( evalArrayIndexTest,
    evalCommaTest,
    evalIdentityTest,
    evalIteratorTest,
    evalObjectIndexTest,
    evalPipeTest,
    evalSliceTest,
  )
import QueryParserTest
  ( arrayIndexParseTest,
    commaParseTest,
    identityParseTest,
    iteratorParseTest,
    objectIndexParseTest,
    pipeParseTest,
    sliceParseTest,
  )
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  -- parser tests
  charTest
  stringTest
  spanPTest
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
  iteratorParseTest
  objectIndexParseTest
  pipeParseTest
  arrayIndexParseTest
  commaParseTest
  sliceParseTest
  -- eval query test
  evalIdentityTest
  evalIteratorTest
  evalObjectIndexTest
  evalPipeTest
  evalArrayIndexTest
  evalCommaTest
  evalSliceTest
