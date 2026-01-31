{-# LANGUAGE OverloadedStrings #-}

module ParserTest where

import Data.Char (isDigit, isLower)
import Parser (Parser (..), many, notFollowedBy, optional, char, spanP, string, satisfy, sepBy, sepBy1, some)
import Test.Hspec

charTest :: Spec
charTest = do
  describe "char Test" $ do
    it "should parse a character" $ do
      runParser (char 'a') "a" 1 1 `shouldBe` Right ("", 1, 2, 'a')

    it "should fail if character is different" $ do
      runParser (char 'a') "b" 1 1 `shouldBe` Left (1, 1, "Expected 'a' but got 'b' on line 1:1.")

    it "should fail on empty string" $ do
      runParser (char 'a') "" 1 1 `shouldBe` Left (1, 1, "Expected 'a' but got empty string on line 1:1.")

stringTest :: Spec
stringTest = do
  describe "string Test" $ do
    it "should parse a string" $ do
      runParser (string "hello") "hello world" 1 1 `shouldBe` Right (" world", 1, 6, "hello")

    it "should fail if string doesn't match. Different character." $ do
      runParser (string "hello") "hollo world" 1 1 `shouldBe` Left (1, 1, "Expected \"hello\" but got \"hollo\" on line 1:1.")

    it "should fail if string doesn't match. Shorter string." $ do
      runParser (string "hello") "hell world" 1 1 `shouldBe` Left (1, 1, "Expected \"hello\" but got \"hell \" on line 1:1.")

    it "should fail on empty string." $ do
      runParser (string "hello") "" 1 1 `shouldBe` Left (1, 1, "Expected \"hello\" but got empty string on line 1:1.")

spanPTest :: Spec
spanPTest = do
  describe "spanP Test" $ do
    it "should parse a spanP of numbers" $
      runParser (spanP isDigit) "123 abc" 1 1 `shouldBe` Right (" abc", 1, 4, "123")

    it "should parse a spanP of lowercase letters" $
      runParser (spanP isLower) "abc 123" 1 1 `shouldBe` Right (" 123", 1, 4, "abc")

    it "should return an empty string if nothing matches" $
      runParser (spanP isDigit) "abc 123" 1 1 `shouldBe` Right ("abc 123", 1, 1, "")

    it "should return an empty string on empty string" $
      runParser (spanP isDigit) "" 1 1 `shouldBe` Right ("", 1, 1, "")

manyTest :: Spec
manyTest = do
  describe "many test" $ do
    it "should parse numbers" $
      runParser (many $ satisfy isDigit) "123 abc" 1 1 `shouldBe` Right (" abc", 1, 4, "123")

    it "should parse numbers" $
      runParser (many $ satisfy isLower) "abc 123" 1 1 `shouldBe` Right (" 123", 1, 4, "abc")

    it "should return empty string if nothing matches" $
      runParser (many $ satisfy isDigit) "abc 123" 1 1 `shouldBe` Right ("abc 123", 1, 1, "")

someTest :: Spec
someTest = do
  describe "some test" $ do
    it "should parse numbers" $
      runParser (some $ satisfy isDigit) "123 abc" 1 1 `shouldBe` Right (" abc", 1, 4, "123")

    it "should parse numbers" $
      runParser (some $ satisfy isLower) "abc 123" 1 1 `shouldBe` Right (" 123", 1, 4, "abc")

    it "should fail if nothing matches" $
      runParser (some $ satisfy isDigit) "abc 123" 1 1 `shouldBe` Left (1, 1, "Unexpected 'a'")

optionalTest :: Spec
optionalTest = do
  describe "optional test" $ do
    it "should parse an existing character" $
      runParser (optional $ char 'a') "abc" 1 1 `shouldBe` Right ("bc", 1, 2, Just 'a')

    it "should return Nothing on a non-existing character" $
      runParser (optional $ char 'a') "bbc" 1 1 `shouldBe` Right ("bbc", 1, 1, Nothing)

notFollowedByTest :: Spec
notFollowedByTest = do
  describe "notFollowedBy test" $ do
    it "should fail on a successful parse" $
      runParser (notFollowedBy $ char 'a') "abc" 1 1 `shouldBe` Left (1, 1, "Unexpected value")

    it "should pass on a failed parse" $
      runParser (notFollowedBy $ char 'a') "bbc" 1 1 `shouldBe` Right ("bbc", 1, 1, ())

satisfyTest :: Spec
satisfyTest = do
  describe "satisfy test" $
    it "should parse a digit" $ do
      runParser (satisfy isDigit) "1" 1 1 `shouldBe` Right ("", 1, 2, '1')

sepByTest :: Spec
sepByTest = do
  describe "sepBy test" $ do
    it "should parse a comma-separated list of numbers" $
      runParser (sepBy (satisfy isDigit) (char ',')) "1,2,3,4" 1 1 `shouldBe` Right ("", 1, 8, ['1', '2', '3', '4'])

    it "should not fail if it doesn't parse" $
      runParser (sepBy (satisfy isDigit) (char ',')) ",2,3,4" 1 1 `shouldBe` Right (",2,3,4", 1, 1, "")

sepBy1Test :: Spec
sepBy1Test = do
  describe "sepBy1 test" $ do
    it "should parse a comma-separated list of numbers" $
      runParser (sepBy1 (satisfy isDigit) (char ',')) "1,2,3,4" 1 1 `shouldBe` Right ("", 1, 8, ['1', '2', '3', '4'])

    it "should fail if it doesn't parse" $
      runParser (sepBy1 (satisfy isDigit) (char ',')) ",2,3,4" 1 1 `shouldBe` Left (1, 1, "Unexpected ','")
