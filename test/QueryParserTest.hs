{-# LANGUAGE OverloadedStrings #-}

module QueryParserTest where

import Parser (Parser (..))
import Query.AST (Query (..))
import Query.Parser (identity)
import Test.Hspec (Spec, describe, it, shouldBe)

identityParseTest :: Spec
identityParseTest = do
  describe "identityParseTest" $ do
    it "should parse a dot" $ do
      runParser identity "." 1 1 `shouldBe` Right ("", 1, 2, Identity)
