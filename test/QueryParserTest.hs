{-# LANGUAGE OverloadedStrings #-}

module QueryParserTest where

import Parser (Parser (..))
import Query.AST (Query (..))
import Query.Parser (arrayIndex, comma, identity, iterator, objectIndex, pipe, slice)
import Test.Hspec (Spec, describe, it, shouldBe)

identityParseTest :: Spec
identityParseTest = do
  describe "identityParseTest" $ do
    it "should parse a dot" $ do
      runParser identity "." 1 1 `shouldBe` Right ("", 1, 2, Identity)

iteratorParseTest :: Spec
iteratorParseTest = do
  describe "iteratorParseTest" $ do
    it "should parse an iterator" $ do
      runParser iterator ".[]" 1 1 `shouldBe` Right ("", 1, 4, Iterator)

    it "should parse an iterator with spaces" $ do
      runParser iterator ".[   ]   " 1 1 `shouldBe` Right ("", 1, 10, Iterator)

    it "should fail on incomplete input" $ do
      runParser iterator ".[" 1 1 `shouldBe` Left (1, 3, "Expected ']' but got empty string on line 1:3.")
      runParser iterator ".]" 1 1 `shouldBe` Left (1, 2, "Expected '[' but got ']' on line 1:2.")

objectIndexParseTest :: Spec
objectIndexParseTest = do
  describe "objectIndexParseTest" $ do
    it "should parse a simple key" $
      runParser objectIndex ".foo" 1 1 `shouldBe` Right ("", 1, 5, ObjectIndex "foo" False)

    it "should parse a simple key with optional modifier" $
      runParser objectIndex ".foo?" 1 1 `shouldBe` Right ("", 1, 6, ObjectIndex "foo" True)

    it "should parse a simple key chain" $
      runParser objectIndex ".foo?.bar" 1 1 `shouldBe` Right ("", 1, 10, Pipe (ObjectIndex "foo" True) (ObjectIndex "bar" False))

    it "should parse a quoted key" $
      runParser objectIndex ".\"foo\\u5797\"" 1 1 `shouldBe` Right ("", 1, 13, ObjectIndex "foo垗" False)

    it "should parse a chain of keys quoted and in brackets" $
      runParser objectIndex ".\"foo\".[\"bar\"]" 1 1 `shouldBe` Right ("", 1, 15, Pipe (ObjectIndex "foo" False) (ObjectIndex "bar" False))

    it "should parse a key between brackets" $
      runParser objectIndex ".[\"foo\"]" 1 1 `shouldBe` Right ("", 1, 9, ObjectIndex "foo" False)

    it "should fail if a key between brackets doesn't have quotes" $
      runParser objectIndex ".[foo]" 1 1 `shouldBe` Left (1, 3, "Expected '\"' but got 'f' on line 1:3.")

    it "should fail if a key between brackets doesn't have a closing bracket" $
      runParser objectIndex ".[\"foo\"" 1 1 `shouldBe` Left (1, 8, "Expected ']' but got empty string on line 1:8.")

pipeParseTest :: Spec
pipeParseTest = do
  describe "pipeParseTest" $ do
    it "should parse a simple pipe of identities" $
      runParser pipe ". | ." 1 1 `shouldBe` Right ("", 1, 6, Pipe Identity Identity)

    it "should parse some pipes of keys" $
      runParser pipe ".user | .address | .street" 1 1 `shouldBe` Right ("", 1, 27, Pipe (ObjectIndex "user" False) (Pipe (ObjectIndex "address" False) (ObjectIndex "street" False)))

    it "should parse a pipe of identity into iterator into key index" $
      runParser pipe ". | .[] | .street" 1 1 `shouldBe` Right ("", 1, 18, Pipe Identity (Pipe Iterator (ObjectIndex "street" False)))

arrayIndexParseTest :: Spec
arrayIndexParseTest = do
  describe "arrayIndexParseTest" $ do
    it "should parse a positive index" $
      runParser arrayIndex ".[0]" 1 1 `shouldBe` Right ("", 1, 5, ArrayIndex 0 False)

    it "should parse a positive index with optional" $
      runParser arrayIndex ".[124]?" 1 1 `shouldBe` Right ("", 1, 8, ArrayIndex 124 True)

    it "should parse a negative index with optional" $
      runParser arrayIndex ".[-5]?" 1 1 `shouldBe` Right ("", 1, 7, ArrayIndex (-5) True)

    it "should fail on incomplete input" $
      runParser arrayIndex ".[1" 1 1 `shouldBe` Left (1, 4, "Expected ']' but got empty string on line 1:4.")

commaParseTest :: Spec
commaParseTest = do
  describe "commaParseTest" $ do
    it "should parse a comma of identities" $
      runParser comma ".,." 1 1 `shouldBe` Right ("", 1, 4, Comma Identity Identity)

    it "should parse a comma of identity and iterator" $
      runParser comma ".     ,    .[]       " 1 1 `shouldBe` Right ("", 1, 22, Comma Identity Iterator)

    it "should parse a comma of array index and pipe" $
      runParser comma ".[0], .[] | .id" 1 1 `shouldBe` Right ("", 1, 16, Comma (ArrayIndex 0 False) (Pipe Iterator (ObjectIndex "id" False)))

sliceParseTest :: Spec
sliceParseTest = do
  describe "sliceParseTest" $ do
    it "should parse a slice with both ends" $
      runParser slice ".[1:-1]" 1 1 `shouldBe` Right ("", 1, 8, Slice {start = Just 1, end = Just (-1)})

    it "should parse a slice with start index" $
      runParser slice ".[1:]" 1 1 `shouldBe` Right ("", 1, 6, Slice {start = Just 1, end = Nothing})

    it "should parse a slice with end index" $
      runParser slice ".[:-1]" 1 1 `shouldBe` Right ("", 1, 7, Slice {start = Nothing, end = Just (-1)})

    it "should reject a slice with no indices" $
      runParser slice ".[:]" 1 1 `shouldBe` Left (1, 5, "Slice requires a start or end index")
