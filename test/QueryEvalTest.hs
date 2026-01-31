{-# LANGUAGE OverloadedStrings #-}

module QueryEvalTest where

import Json.AST (Json (..))
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
