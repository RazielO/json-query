module JsonParserTest where

import Parser (Parser (..))
import JsonParser (JsonValue (..), jsonBool, jsonNull, jsonNumber)
import Test.Hspec

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

jsonNumberTest :: Spec
jsonNumberTest = do
  describe "jsonNumber Test" $ do
    it "should parse valid integers" $ do
      runParser jsonNumber "0" 1 1 `shouldBe` Right ("", 1, 2, JsonInteger 0)
      runParser jsonNumber "5" 1 1 `shouldBe` Right ("", 1, 2, JsonInteger 5)
      runParser jsonNumber "42" 1 1 `shouldBe` Right ("", 1, 3, JsonInteger 42)
      runParser jsonNumber "-7" 1 1 `shouldBe` Right ("", 1, 3, JsonInteger (-7))
      runParser jsonNumber "-123" 1 1 `shouldBe` Right ("", 1, 5, JsonInteger (-123))

    it "should fail on invalid integers" $ do
      runParser jsonNumber "01" 1 1 `shouldBe` Left (1, 2, "Unexpected value")
      runParser jsonNumber "-01" 1 1 `shouldBe` Left (1, 3, "Unexpected value")
      runParser jsonNumber "00" 1 1 `shouldBe` Left (1, 2, "Unexpected value")
      runParser jsonNumber "0123" 1 1 `shouldBe` Left (1, 2, "Unexpected value")
      runParser jsonNumber "-09" 1 1 `shouldBe` Left (1, 3, "Unexpected value")

    it "should parse valid floats" $ do
      runParser jsonNumber "0.0" 1 1 `shouldBe` Right ("", 1, 4, JsonDouble 0.0)
      runParser jsonNumber "3.1415" 1 1 `shouldBe` Right ("", 1, 7, JsonDouble 3.1415)
      runParser jsonNumber "-2.5E+10" 1 1 `shouldBe` Right ("", 1, 9, JsonDouble (-2.5e10))
      runParser jsonNumber "42.0e-3" 1 1 `shouldBe` Right ("", 1, 8, JsonDouble 4.2e-2)
      runParser jsonNumber "-7.123e0" 1 1 `shouldBe` Right ("", 1, 9, JsonDouble (-7.123))

    it "should fail on invalid floats" $ do
      runParser jsonNumber ".5" 1 1 `shouldBe` Left (1, 1, "Expected '0' but got '.' on line 1:1.")
      runParser jsonNumber "+.25" 1 1 `shouldBe` Left (1, 1, "Expected '0' but got '+' on line 1:1.")
