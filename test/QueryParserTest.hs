{-# LANGUAGE OverloadedStrings #-}

module QueryParserTest (queryParserTests) where

import Parser (Parser (..))
import Query.AST (Query (..))
import Query.Parser (arrayIndex, comma, dotExpr, iterator, objectIndex, pipe, slice)
import Test.Hspec (Spec, describe, it, shouldBe)

queryParserTests :: Spec
queryParserTests = do
  identityParseTest
  iteratorParseTest
  objectIndexParseTest
  pipeParseTest
  arrayIndexParseTest
  commaParseTest
  sliceParseTest

identityParseTest :: Spec
identityParseTest = do
  describe "Query.Parser. Parser for identity" $ do
    it "should parse a dot" $ do
      runParser dotExpr "." 1 1 `shouldBe` Right ("", 1, 2, Identity)

iteratorParseTest :: Spec
iteratorParseTest = do
  describe "Query.Parser. Parser for iterator" $ do
    it "should parse an iterator" $ do
      runParser iterator "[]" 1 1 `shouldBe` Right ("", 1, 3, Iterator False)

    it "should parse an optional iterator" $ do
      runParser iterator "[]?" 1 1 `shouldBe` Right ("", 1, 4, Iterator True)

    it "should parse an iterator with spaces" $ do
      runParser iterator "[   ]   " 1 1 `shouldBe` Right ("", 1, 9, Iterator False)

    it "should fail on incomplete input" $ do
      runParser iterator "[" 1 1 `shouldBe` Left (1, 2, "Expected ']' but got empty string on line 1:2.")
      runParser iterator "]" 1 1 `shouldBe` Left (1, 1, "Expected '[' but got ']' on line 1:1.")

objectIndexParseTest :: Spec
objectIndexParseTest = do
  describe "Query.Parser. Parser for objectIndex" $ do
    it "should parse a simple key" $
      runParser objectIndex "foo" 1 1 `shouldBe` Right ("", 1, 4, ObjectIndex "foo" False)

    it "should parse a simple key with optional modifier" $
      runParser objectIndex "foo?" 1 1 `shouldBe` Right ("", 1, 5, ObjectIndex "foo" True)

    it "should parse a quoted key" $
      runParser objectIndex "\"foo\\u5797\"" 1 1 `shouldBe` Right ("", 1, 12, ObjectIndex "foo垗" False)

    it "should parse a key between brackets" $
      runParser objectIndex "[\"foo\"]" 1 1 `shouldBe` Right ("", 1, 8, ObjectIndex "foo" False)

    it "should fail if a key between brackets doesn't have quotes" $
      runParser objectIndex "[foo]" 1 1 `shouldBe` Left (1, 2, "Expected '\"' but got 'f' on line 1:2.")

    it "should fail if a key between brackets doesn't have a closing bracket" $
      runParser objectIndex "[\"foo\"" 1 1 `shouldBe` Left (1, 7, "Expected ']' but got empty string on line 1:7.")

pipeParseTest :: Spec
pipeParseTest = do
  describe "Query.Parser. Parser for pipe" $ do
    it "should parse a simple pipe of identities" $
      runParser pipe ". | ." 1 1 `shouldBe` Right ("", 1, 6, Pipe Identity Identity)

    it "should parse some pipes of keys" $
      runParser pipe ".user | .address | .street" 1 1 `shouldBe` Right ("", 1, 27, Pipe (Pipe (ObjectIndex "user" False) (ObjectIndex "address" False)) (ObjectIndex "street" False))

    it "should parse a pipe of identity into iterator into key index" $
      runParser pipe ". | .[] | .street" 1 1 `shouldBe` Right ("", 1, 18, Pipe (Pipe Identity (Iterator False)) (ObjectIndex "street" False))

arrayIndexParseTest :: Spec
arrayIndexParseTest = do
  describe "Query.Parser. Parser for arrayIndex" $ do
    it "should parse a positive index" $
      runParser arrayIndex "[0]" 1 1 `shouldBe` Right ("", 1, 4, ArrayIndex 0 False)

    it "should parse a positive index with optional" $
      runParser arrayIndex "[124]?" 1 1 `shouldBe` Right ("", 1, 7, ArrayIndex 124 True)

    it "should parse a negative index with optional" $
      runParser arrayIndex "[-5]?" 1 1 `shouldBe` Right ("", 1, 6, ArrayIndex (-5) True)

    it "should fail on incomplete input" $
      runParser arrayIndex "[1" 1 1 `shouldBe` Left (1, 3, "Expected ']' but got empty string on line 1:3.")

commaParseTest :: Spec
commaParseTest = do
  describe "Query.Parser. Parser for comma" $ do
    it "should parse a comma of identities" $
      runParser comma ".,." 1 1 `shouldBe` Right ("", 1, 4, Comma Identity Identity)

    it "should parse a comma of identity and iterator" $
      runParser comma ".     ,    .[]       " 1 1 `shouldBe` Right ("", 1, 22, Comma Identity (Iterator False))

    it "should parse a comma of array index and pipe" $
      runParser comma ".[0], .[] | .id" 1 1 `shouldBe` Right ("", 1, 16, Comma (ArrayIndex 0 False) (Pipe (Iterator False) (ObjectIndex "id" False)))

sliceParseTest :: Spec
sliceParseTest = do
  describe "Query.Parser. Parser for slice" $ do
    it "should parse a slice with both ends" $
      runParser slice "[1:-1]" 1 1 `shouldBe` Right ("", 1, 7, Slice {start = Just 1, end = Just (-1)})

    it "should parse a slice with start index" $
      runParser slice "[1:]" 1 1 `shouldBe` Right ("", 1, 5, Slice {start = Just 1, end = Nothing})

    it "should parse a slice with end index" $
      runParser slice "[:-1]" 1 1 `shouldBe` Right ("", 1, 6, Slice {start = Nothing, end = Just (-1)})

    it "should reject a slice with no indices" $
      runParser slice "[:]" 1 1 `shouldBe` Left (1, 4, "Slice requires a start or end index")
