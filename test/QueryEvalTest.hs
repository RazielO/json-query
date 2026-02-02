{-# LANGUAGE OverloadedStrings #-}

module QueryEvalTest where

import Data.Either (fromRight)
import Json.AST (Json (..))
import Json.Parser (json)
import Parser (Parser (runParser))
import Query.AST (Query (..))
import Query.Eval (evalQuery)
import Test.Hspec (Spec, describe, it, shouldBe)

evalIdentityTest :: Spec
evalIdentityTest = do
  describe "evalIdentityTest" $ do
    it "should return the same json as the input" $ do
      evalQuery Identity Null `shouldBe` Right [Null]
      evalQuery Identity (Number 1) `shouldBe` Right [Number 1]
      evalQuery Identity (Str "Hello, World!") `shouldBe` Right [Str "Hello, World!"]

evalIteratorTest :: Spec
evalIteratorTest = do
  describe "evalIteratorTest" $ do
    it "should iterate over an empty array" $ do
      let (_, _, _, json') = fromRight ("", 1, 1, Null) (runParser json "[]" 1 1)
      evalQuery Iterator json' `shouldBe` Right []

    it "should iterate over an array" $ do
      let (_, _, _, json') = fromRight ("", 1, 1, Null) (runParser json "[1,2,null]" 1 1)
      evalQuery Iterator json' `shouldBe` Right [Number 1, Number 2, Null]

    it "should iterate over an empty object" $ do
      let (_, _, _, json') = fromRight ("", 1, 1, Null) (runParser json "{}" 1 1)
      evalQuery Iterator json' `shouldBe` Right []

    it "should iterate over the values of an object" $ do
      let (_, _, _, json') = fromRight ("", 1, 1, Null) (runParser json "{\"foo\": 1, \"bar\": \"abc\"}" 1 1)
      evalQuery Iterator json' `shouldBe` Right [Number 1, Str "abc"]

evalObjectIndexTest :: Spec
evalObjectIndexTest = do
  describe "evalObjectIndexTest" $ do
    it "should return a key if it's on the object" $ do
      let (_, _, _, json') = fromRight ("", 1, 1, Null) (runParser json "{\"foo\": 1, \"bar\": \"abc\"}" 1 1)
      evalQuery (ObjectIndex "foo" False) json' `shouldBe` Right [Number 1]
      evalQuery (ObjectIndex "foo" True) json' `shouldBe` Right [Number 1]

    it "should return null if the key is not on the object" $ do
      let (_, _, _, json') = fromRight ("", 1, 1, Null) (runParser json "{\"foo\": 1, \"bar\": \"abc\"}" 1 1)
      evalQuery (ObjectIndex "hello" True) json' `shouldBe` Right [Null]

    it "should fail if the JSON is not an object and it's not optional" $ do
      let (_, _, _, json') = fromRight ("", 1, 1, Null) (runParser json "[]" 1 1)
      evalQuery (ObjectIndex "hello" False) json' `shouldBe` Left "Cannot index an array with a string key"

    it "should not fail if the JSON is not an object and it's optional" $ do
      let (_, _, _, json') = fromRight ("", 1, 1, Null) (runParser json "[]" 1 1)
      evalQuery (ObjectIndex "hello" True) json' `shouldBe` Right []

evalPipeTest :: Spec
evalPipeTest = do
  describe "evalPipeTest" $ do
    it "should evaluate a pipe of identities" $
      evalQuery (Pipe Identity Identity) (Number 1) `shouldBe` Right [Number 1]

    it "should evaluate a pipe identity > object key" $ do
      let (_, _, _, json') = fromRight ("", 1, 1, Null) (runParser json "{\"foo\": 1, \"bar\": \"abc\"}" 1 1)
      evalQuery (Pipe Identity (ObjectIndex "foo" False)) json' `shouldBe` Right [Number 1]

    it "should evaluate a pipe iterator > object key" $ do
      let (_, _, _, json') = fromRight ("", 1, 1, Null) (runParser json "[{\"foo\": 1}, {\"foo\": 2}, {\"foo\": \"abc\"}, {\"bar\": 1}]" 1 1)
      evalQuery (Pipe Iterator (ObjectIndex "foo" True)) json' `shouldBe` Right [Number 1, Number 2, Str "abc", Null]

    it "should fail if any operation is not valid (index a number)" $ do
      let (_, _, _, json') = fromRight ("", 1, 1, Null) (runParser json "{\"foo\": 1}" 1 1)
      evalQuery (Pipe (ObjectIndex "foo" True) (ObjectIndex "bar" False)) json' `shouldBe` Left "Cannot index a number with a string key"

evalArrayIndexTest :: Spec
evalArrayIndexTest = do
  describe "evalArrayIndexTest" $ do
    it "should correctly evaluate a positive index" $ do
      let (_, _, _, json') = fromRight ("", 1, 1, Null) (runParser json "[1,2,3,4,5]" 1 1)
      evalQuery (ArrayIndex 0 False) json' `shouldBe` Right [Number 1]

    it "should correctly evaluate a negative index" $ do
      let (_, _, _, json') = fromRight ("", 1, 1, Null) (runParser json "[1,2,3,4,5]" 1 1)
      evalQuery (ArrayIndex (-1) False) json' `shouldBe` Right [Number 5]

    it "should return null if index is out of bounds" $ do
      let (_, _, _, json') = fromRight ("", 1, 1, Null) (runParser json "[1,2,3,4,5]" 1 1)
      evalQuery (ArrayIndex 6 True) json' `shouldBe` Right [Null]
      evalQuery (ArrayIndex (-8) True) json' `shouldBe` Right [Null]

    it "should fail if indexed JSON is not an array" $ do
      evalQuery (ArrayIndex 6 False) Null `shouldBe` Left "Cannot index null with a number"
      evalQuery (ArrayIndex 6 False) (Number 1) `shouldBe` Left "Cannot index a number with a number"

evalCommaTest :: Spec
evalCommaTest = do
  describe "commaEvalTest" $ do
    it "should correctly evaluate a simple comma" $ do
      let (_, _, _, json') = fromRight ("", 1, 1, Null) (runParser json "1" 1 1)
      evalQuery (Comma Identity Identity) json' `shouldBe` Right [Number 1, Number 1]

    it "should correctly evaluate a comma of different types" $ do
      let (_, _, _, json') = fromRight ("", 1, 1, Null) (runParser json "[1,2,3]" 1 1)
      evalQuery (Comma Identity Iterator) json' `shouldBe` Right [Array [Number 1, Number 2, Number 3], Number 1, Number 2, Number 3]

    it "should fail if any of the operations fail" $ do
      let (_, _, _, json') = fromRight ("", 1, 1, Null) (runParser json "[1,2,3]" 1 1)
      evalQuery (Comma Identity (ObjectIndex "foo" False)) json' `shouldBe` Left "Cannot index an array with a string key"

evalSliceTest :: Spec
evalSliceTest = do
  describe "sliceEvalTest" $ do
    it "should correctly evaluate slice with both indices" $ do
      let (_, _, _, json') = fromRight ("", 1, 1, Null) (runParser json "[1,2,3,4,5]" 1 1)
      evalQuery (Slice {start = Just 1, end = Just (-1)}) json' `shouldBe` Right [Array [Number 2, Number 3, Number 4]]
      let (_, _, _, json'') = fromRight ("", 1, 1, Null) (runParser json "\"abcde\"" 1 1)
      evalQuery (Slice {start = Just 1, end = Just (-1)}) json'' `shouldBe` Right [Str "bcd"]

    it "should correctly evaluate slice with only start index" $ do
      let (_, _, _, json') = fromRight ("", 1, 1, Null) (runParser json "[1,2,3,4,5]" 1 1)
      evalQuery (Slice {start = Just (-2), end = Nothing}) json' `shouldBe` Right [Array [Number 4, Number 5]]
      let (_, _, _, json'') = fromRight ("", 1, 1, Null) (runParser json "\"abcde\"" 1 1)
      evalQuery (Slice {start = Just (-2), end = Nothing}) json'' `shouldBe` Right [Str "de"]

    it "should correctly evaluate slice with only end index" $ do
      let (_, _, _, json') = fromRight ("", 1, 1, Null) (runParser json "[1,2,3,4,5]" 1 1)
      evalQuery (Slice {start = Nothing, end = Just 3}) json' `shouldBe` Right [Array [Number 1, Number 2, Number 3]]
      let (_, _, _, json'') = fromRight ("", 1, 1, Null) (runParser json "\"abcde\"" 1 1)
      evalQuery (Slice {start = Nothing, end = Just 3}) json'' `shouldBe` Right [Str "abc"]

    it "should fail if sliced element is not array or string" $ do
      evalQuery (Slice {start = Nothing, end = Just 3}) Null `shouldBe` Left "Cannot slice null"
      evalQuery (Slice {start = Nothing, end = Just 3}) (Number 1) `shouldBe` Left "Cannot slice a number"
