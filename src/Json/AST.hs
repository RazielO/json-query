{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Json.AST
  ( Json (..),
    prettyDisplay,
    name
  )
where

import Data.Char (toLower)
import Data.HashMap.Strict (HashMap)
import Data.List (intercalate)
import Data.Scientific (FPFormat (Generic), Scientific, formatScientific, isInteger)
import Data.Text (Text)
import qualified Data.Text as Text (concat)
import Prettyprinter (Doc, Pretty (pretty), annotate, comma, enclose, indent, lbrace, lbracket, punctuate, rbrace, rbracket, vsep, (<+>), line)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), bold, color, colorDull)
import Text.Printf (printf)

-- | Top-level JSON elements
data Json
  = Object {objectList :: [(Text, Json)], objectMap :: HashMap Text Json}
  | Array [Json]
  | Str Text
  | Number Scientific
  | Boolean Bool
  | Null
  deriving (Eq)

instance Show Json where
  show :: Json -> String
  show = flip show' 0
    where
      show' :: Json -> Int -> String
      show' value n =
        let indentation = replicate (n * 2) ' '
            showScalar = case value of
              Null -> "null"
              Boolean bool' -> map toLower (show bool')
              Number num ->
                let decimals = if isInteger num then Just 0 else Nothing
                 in formatScientific Generic decimals num
              Str str -> show str
              _ -> error "not a scalar"
         in case value of
              Array arr
                | null arr -> "[]"
                | otherwise ->
                    let elements = map (\value' -> replicate ((n + 1) * 2) ' ' ++ show' value' (n + 1)) arr
                     in printf "[\n%s\n%s]" (intercalate ",\n" elements) indentation
              Object obj _ ->
                if null obj
                  then "{}"
                  else
                    let pairs =
                          map
                            ( \(k, v) ->
                                replicate ((n + 1) * 2) ' ' ++ show k ++ ": " ++ show' v (n + 1)
                            )
                            obj
                     in printf "{\n%s\n%s}" (intercalate ",\n" pairs) indentation
              _ -> showScalar

prettyDisplay :: Json -> Doc AnsiStyle
prettyDisplay value = prettyDisplay' value 1 <> line

boldBlue :: AnsiStyle
boldBlue = color Blue <> bold

comma' :: Doc AnsiStyle
comma' = annotate boldBlue comma

prettyDisplay' :: Json -> Int -> Doc AnsiStyle
prettyDisplay' Null _ = annotate (colorDull Black) "null"
prettyDisplay' (Boolean value) _ = annotate (color White) (pretty $ map toLower (show value))
prettyDisplay' (Number value) _ =
  let decimals = if isInteger value then Just 0 else Nothing
      string' = formatScientific Generic decimals value
   in annotate (color White) (pretty string')
prettyDisplay' (Str string') _ = annotate (colorDull Green) (pretty $ Text.concat ["\"", string', "\""])
prettyDisplay' (Array values) level =
  let start = annotate boldBlue lbracket
      end = annotate boldBlue rbracket
      elements = punctuate comma' (map (\v -> prettyDisplay' v (level + 1)) values)
   in case values of
        [] -> start <> end
        _ -> vsep [start, indent 2 (vsep elements), end]
prettyDisplay' (Object object _) level =
  let start = annotate (color Blue <> bold) lbrace
      end = annotate (color Blue <> bold) rbrace
      elems = punctuate comma (map (\(k, v) -> annotate boldBlue (enclose "\"" "\":" (pretty k)) <+> prettyDisplay' v (level + 1)) object)
   in case object of
        [] -> start <> end
        _ -> vsep [start, indent 2 (vsep elems), end]

name :: Json -> String
name (Object _ _) = "an object"
name (Array _) = "an array"
name (Str _) = "a string"
name (Number _) = "a number"
name (Boolean _) = "a boolean"
name Null = "null"
