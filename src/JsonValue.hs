{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module JsonValue
  ( JsonValue (..),
    prettyDisplay,
  )
where

import Data.Char (toLower)
import Data.List (intercalate)
import Data.Scientific (FPFormat (Generic), Scientific, formatScientific, isInteger)
import Data.Text (Text)
import qualified Data.Text as Text (concat)
import Prettyprinter (Doc, Pretty (pretty), annotate, comma, enclose, indent, lbrace, lbracket, punctuate, rbrace, rbracket, vsep, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), bold, color, colorDull)
import Text.Printf (printf)

-- | Top-level JSON elements
data JsonValue
  = Object [(Text, JsonValue)]
  | Array [JsonValue]
  | Str Text
  | Number Scientific
  | Boolean Bool
  | Null
  deriving (Eq)

instance Show JsonValue where
  show :: JsonValue -> String
  show = flip show' 0
    where
      show' :: JsonValue -> Int -> String
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
              Object obj ->
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

prettyDisplay :: JsonValue -> Doc AnsiStyle
prettyDisplay value = prettyDisplay' value 1

boldBlue :: AnsiStyle
boldBlue = color Blue <> bold

comma' :: Doc AnsiStyle
comma' = annotate boldBlue comma

prettyDisplay' :: JsonValue -> Int -> Doc AnsiStyle
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
prettyDisplay' (Object object) level =
  let start = annotate (color Blue <> bold) lbrace
      end = annotate (color Blue <> bold) rbrace
      elems = punctuate comma (map (\(k, v) -> annotate boldBlue (enclose "\"" "\":" (pretty k)) <+> prettyDisplay' v (level + 1)) object)
   in case object of
        [] -> start <> end
        _ -> vsep [start, indent 2 (vsep elems), end]