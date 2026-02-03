{-# LANGUAGE OverloadedStrings #-}

module JsonParserTest (jsonParserTests) where

import Json.AST (Json (..))
import Json.Parser (jsonArray, jsonBool, jsonNull, jsonNumber, jsonObject, jsonString)
import Parser (Parser (..))
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

jsonParserTests :: Spec
jsonParserTests = do
  jsonNullTest
  jsonBoolTest
  jsonNumberTest
  jsonArrayTest
  jsonStringTest
  jsonObjectTest

jsonNullTest :: Spec
jsonNullTest = do
  describe "JSON.Parser. Parser for JSON null" $ do
    it "should parse a null value" $ do
      runParser jsonNull "null" 1 1 `shouldBe` Right ("", 1, 5, Null)

    it "should fail on other strings" $ do
      runParser jsonNull "nall" 1 1 `shouldBe` Left (1, 1, "Expected \"null\" but got \"nall\" on line 1:1.")
      runParser jsonNull "nul" 1 1 `shouldBe` Left (1, 1, "Expected \"null\" but got \"nul\" on line 1:1.")

    it "should fail on empty string" $ do
      runParser jsonNull "" 1 1 `shouldBe` Left (1, 1, "Expected \"null\" but got empty string on line 1:1.")

jsonBoolTest :: Spec
jsonBoolTest = do
  describe "JSON.Parser. Parser for JSON boolean" $ do
    it "should parse a bool value" $ do
      runParser jsonBool "true" 1 1 `shouldBe` Right ("", 1, 5, Boolean True)
      runParser jsonBool "false" 1 1 `shouldBe` Right ("", 1, 6, Boolean False)

    it "should fail on other strings" $ do
      runParser jsonBool "trua" 1 1 `shouldBe` Left (1, 1, "Expected \"true\" but got \"trua\" on line 1:1.")
      runParser jsonBool "fals" 1 1 `shouldBe` Left (1, 1, "Expected \"true\" but got \"fals\" on line 1:1.")

    it "should fail on empty string" $ do
      runParser jsonBool "" 1 1 `shouldBe` Left (1, 1, "Expected \"true\" but got empty string on line 1:1.")

jsonNumberTest :: Spec
jsonNumberTest = do
  describe "JSON.Parser. Parser for JSON number" $ do
    it "should parse valid integers" $ do
      runParser jsonNumber "0" 1 1 `shouldBe` Right ("", 1, 2, Number 0)
      runParser jsonNumber "5" 1 1 `shouldBe` Right ("", 1, 2, Number 5)
      runParser jsonNumber "42" 1 1 `shouldBe` Right ("", 1, 3, Number 42)
      runParser jsonNumber "-7" 1 1 `shouldBe` Right ("", 1, 3, Number (-7))
      runParser jsonNumber "-123" 1 1 `shouldBe` Right ("", 1, 5, Number (-123))

    it "should fail on invalid integers" $ do
      runParser jsonNumber "01" 1 1 `shouldBe` Left (1, 2, "Unexpected value")
      runParser jsonNumber "-01" 1 1 `shouldBe` Left (1, 3, "Unexpected value")
      runParser jsonNumber "00" 1 1 `shouldBe` Left (1, 2, "Unexpected value")
      runParser jsonNumber "0123" 1 1 `shouldBe` Left (1, 2, "Unexpected value")
      runParser jsonNumber "-09" 1 1 `shouldBe` Left (1, 3, "Unexpected value")

    it "should parse valid floats" $ do
      runParser jsonNumber "0.0" 1 1 `shouldBe` Right ("", 1, 4, Number 0.0)
      runParser jsonNumber "3.1415" 1 1 `shouldBe` Right ("", 1, 7, Number 3.1415)
      runParser jsonNumber "-2.5E+10" 1 1 `shouldBe` Right ("", 1, 9, Number (-2.5e10))
      runParser jsonNumber "42.0e-3" 1 1 `shouldBe` Right ("", 1, 8, Number 4.2e-2)
      runParser jsonNumber "-7.123e0" 1 1 `shouldBe` Right ("", 1, 9, Number (-7.123))

    it "should fail on invalid floats" $ do
      runParser jsonNumber ".5" 1 1 `shouldBe` Left (1, 1, "Expected '0' but got '.' on line 1:1.")
      runParser jsonNumber "+.25" 1 1 `shouldBe` Left (1, 1, "Expected '0' but got '+' on line 1:1.")

jsonArrayTest :: Spec
jsonArrayTest = do
  describe "JSON.Parser. Parser for JSON array" $ do
    it "should parse an empty array" $
      runParser jsonArray "[]" 1 1 `shouldBe` Right ("", 1, 3, Array [])

    it "should parse an array with same types of values" $
      runParser jsonArray "[1 , 2, 3 ]" 1 1 `shouldBe` Right ("", 1, 12, Array [Number 1, Number 2, Number 3])

    it "should parse an array with different types of values" $
      runParser jsonArray "[1, true, null]" 1 1 `shouldBe` Right ("", 1, 16, Array [Number 1, Boolean True, Null])

    it "should parse nested arrays" $
      runParser jsonArray "[[], [true, null]]" 1 1 `shouldBe` Right ("", 1, 19, Array [Array [], Array [Boolean True, Null]])

jsonStringTest :: Spec
jsonStringTest = do
  describe "JSON.Parser. Parser for JSON string" $ do
    it "should parse an empty string" $
      runParser jsonString "\"\"" 1 1 `shouldBe` Right ("", 1, 3, Str "")

    it "should parse a string" $
      runParser jsonString "\"Hello, world!\"" 1 1 `shouldBe` Right ("", 1, 16, Str "Hello, world!")

    it "should parse a string with a line break" $
      runParser jsonString "\"Line\\nBreak\"" 1 1 `shouldBe` Right ("", 1, 14, Str "Line\nBreak")

    it "should parse a string with escaped quotes" $
      runParser jsonString "\"Quote: \\\"\"" 1 1 `shouldBe` Right ("", 1, 12, Str "Quote: \"")

    it "should parse a string with an escaped hex value" $
      runParser jsonString "\"Unicode: \\u00A9\"" 1 1 `shouldBe` Right ("", 1, 18, Str "Unicode: ©")

    it "should fail on a missing closing quote" $
      runParser jsonString "\"Unclosed string" 1 1 `shouldBe` Left (1, 17, "Expected '\"' but got empty string on line 1:17.")

    it "should fail on an invalid escape sequence" $
      runParser jsonString "\"Bad\\xEscape\"" 1 1 `shouldBe` Left (1, 5, "Expected '\"' but got '\\' on line 1:5.")

    it "should fail on an invalid hex sequence" $
      runParser jsonString "\"Control\\u000\"" 1 1 `shouldBe` Left (1, 9, "Expected '\"' but got '\\' on line 1:9.")

    it "should fail on an invalid character (backslash)" $
      runParser jsonString "\"\\\"" 1 1 `shouldBe` Left (1, 4, "Expected '\"' but got empty string on line 1:4.")

jsonObjectTest :: Spec
jsonObjectTest = do
  describe "JSON.Parser. Parser for JSON object" $ do
    it "should parse an empty object" $
      case runParser jsonObject "{}" 1 1 of
        Left (_, _, msg) -> expectationFailure $ "Error while parsing the object: " ++ msg
        Right ("", 1, 3, Object list' _) -> list' `shouldBe` []
        _ -> expectationFailure "Incorrect object parsing"

    it "should parse a JSON object" $
      case runParser jsonObject "{\"name\":\"John Doe\", \"age\": 22, \"drinksCoffee\": true, \"colors\" : [\"red\"]}" 1 1 of
        Left (_, _, msg) -> expectationFailure $ "Error while parsing the object: " ++ msg
        Right ("", 1, 73, Object list' _) -> list' `shouldBe` [("name", Str "John Doe"), ("age", Number 22), ("drinksCoffee", Boolean True), ("colors", Array [Str "red"])]
        _ -> expectationFailure "Incorrect object parsing"

    it "should fail on a missing value" $
      runParser jsonObject "{\"a\":}" 1 1 `shouldBe` Left (1, 2, "Expected \"}\" but got \"\"\" on line 1:2.")

    it "should fail on unquoted key" $
      runParser jsonObject "{a:1}" 1 1 `shouldBe` Left (1, 2, "Expected \"}\" but got \"a\" on line 1:2.")

    it "should fail on invalid value" $ do
      runParser jsonObject "{\"x\":[1,2,]}" 1 1 `shouldBe` Left (1, 2, "Expected \"}\" but got \"\"\" on line 1:2.")
      runParser jsonObject "{\"num\":01}" 1 1 `shouldBe` Left (1, 2, "Expected \"}\" but got \"\"\" on line 1:2.")
