module ParserTest where

import Data.Char (isDigit, isLower)
import Parser (Parser (..), many, notFollowedBy, optional, parseChar, parseSpan, parseString, satisfy, sepBy, sepBy1, some)
import Test.Hspec

parseCharTest :: Spec
parseCharTest = do
  describe "parseChar Test" $ do
    it "should parse a character" $ do
      runParser (parseChar 'a') "a" 1 1 `shouldBe` Right ("", 1, 2, 'a')

    it "should fail if character is different" $ do
      runParser (parseChar 'a') "b" 1 1 `shouldBe` Left (1, 1, "Expected 'a' but got 'b' on line 1:1.")

    it "should fail on empty string" $ do
      runParser (parseChar 'a') "" 1 1 `shouldBe` Left (1, 1, "Expected 'a' but got empty string on line 1:1.")

parseStringTest :: Spec
parseStringTest = do
  describe "parseString Test" $ do
    it "should parse a string" $ do
      runParser (parseString "hello") "hello world" 1 1 `shouldBe` Right (" world", 1, 6, "hello")

    it "should fail if string doesn't match. Different character." $ do
      runParser (parseString "hello") "hollo world" 1 1 `shouldBe` Left (1, 1, "Expected \"hello\" but got \"hollo\" on line 1:1")

    it "should fail if string doesn't match. Shorter string." $ do
      runParser (parseString "hello") "hell world" 1 1 `shouldBe` Left (1, 1, "Expected \"hello\" but got \"hell \" on line 1:1")

    it "should fail on empty string." $ do
      runParser (parseString "hello") "" 1 1 `shouldBe` Left (1, 1, "Expected \"hello\" but got empty string on line 1:1")

parseSpanTest :: Spec
parseSpanTest = do
  describe "parseSpan Test" $ do
    it "should parse a span of numbers" $
      runParser (parseSpan isDigit) "123 abc" 1 1 `shouldBe` Right (" abc", 1, 4, "123")

    it "should parse a span of lowercase letters" $
      runParser (parseSpan isLower) "abc 123" 1 1 `shouldBe` Right (" 123", 1, 4, "abc")

    it "should return an empty string if nothing matches" $
      runParser (parseSpan isDigit) "abc 123" 1 1 `shouldBe` Right ("abc 123", 1, 1, "")

    it "should return an empty string on empty string" $
      runParser (parseSpan isDigit) "" 1 1 `shouldBe` Right ("", 1, 1, "")

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
      runParser (optional $ parseChar 'a') "abc" 1 1 `shouldBe` Right ("bc", 1, 2, Just 'a')

    it "should return Nothing on a non-existing character" $
      runParser (optional $ parseChar 'a') "bbc" 1 1 `shouldBe` Right ("bbc", 1, 1, Nothing)

notFollowedByTest :: Spec
notFollowedByTest = do
  describe "notFollowedBy test" $ do
    it "should fail on a successful parse" $
      runParser (notFollowedBy $ parseChar 'a') "abc" 1 1 `shouldBe` Left (1, 1, "Unexpected value")

    it "should pass on a failed parse" $
      runParser (notFollowedBy $ parseChar 'a') "bbc" 1 1 `shouldBe` Right ("bbc", 1, 1, ())

satisfyTest :: Spec
satisfyTest = do
  describe "satisfy test" $
    it "should parse a digit" $ do
      runParser (satisfy isDigit) "1" 1 1 `shouldBe` Right ("", 1, 2, '1')

sepByTest :: Spec
sepByTest = do
  describe "sepBy test" $ do
    it "should parse a comma-separated list of numbers" $
      runParser (sepBy (satisfy isDigit) (parseChar ',')) "1,2,3,4" 1 1 `shouldBe` Right ("", 1, 8, ['1', '2', '3', '4'])

    it "should not fail if it doesn't parse" $
      runParser (sepBy (satisfy isDigit) (parseChar ',')) ",2,3,4" 1 1 `shouldBe` Right (",2,3,4", 1, 1, "")

sepBy1Test :: Spec
sepBy1Test = do
  describe "sepBy1 test" $ do
    it "should parse a comma-separated list of numbers" $
      runParser (sepBy1 (satisfy isDigit) (parseChar ',')) "1,2,3,4" 1 1 `shouldBe` Right ("", 1, 8, ['1', '2', '3', '4'])

    it "should fail if it doesn't parse" $
      runParser (sepBy1 (satisfy isDigit) (parseChar ',')) ",2,3,4" 1 1 `shouldBe` Left (1, 1, "Unexpected ','")