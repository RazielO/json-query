module JsonParserTest where

import JsonParser (JsonValue (..), Parser (..), jsonBool, jsonNull, parseChar, parseString)
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

jsonNullTest :: Spec
jsonNullTest = do
  describe "jsonNull Test" $ do
    it "should parse a null value" $ do
      runParser jsonNull "null" 1 1 `shouldBe` Right ("", 1, 5, JsonNull)

    it "should fail on other strings" $ do
      runParser jsonNull "nall" 1 1 `shouldBe` Left (1, 1, "Expected \"null\" but got \"nall\" on line 1:1")
      runParser jsonNull "nul" 1 1 `shouldBe` Left (1, 1, "Expected \"null\" but got \"nul\" on line 1:1")

    it "should fail on empty string" $ do
      runParser jsonNull "" 1 1 `shouldBe` Left (1, 1, "Expected \"null\" but got empty string on line 1:1")

jsonBoolTest :: Spec
jsonBoolTest = do
  describe "jsonBool Test" $ do
    it "should parse a bool value" $ do
      runParser jsonBool "true" 1 1 `shouldBe` Right ("", 1, 5, JsonBool True)
      runParser jsonBool "false" 1 1 `shouldBe` Right ("", 1, 6, JsonBool False)

    it "should fail on other strings" $ do
      runParser jsonBool "trua" 1 1 `shouldBe` Left (1, 1, "Expected \"true\" but got \"trua\" on line 1:1")
      runParser jsonBool "fals" 1 1 `shouldBe` Left (1, 1, "Expected \"true\" but got \"fals\" on line 1:1")

    it "should fail on empty string" $ do
      runParser jsonBool "" 1 1 `shouldBe` Left (1, 1, "Expected \"true\" but got empty string on line 1:1")
