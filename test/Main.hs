module Main (main) where

import JsonParserTest
import Test.Hspec

main :: IO ()
main = hspec $ do
  parseCharTest
  parseStringTest
  jsonNullTest
  jsonBoolTest
