module Query.Eval (evalQuery) where

import Data.Either (partitionEithers)
import Data.HashMap.Strict ((!?))
import Data.Text (Text)
import qualified Data.Text as Text (length, pack, unpack)
import Json.AST (Json (..), name)
import Query.AST (Query (..))
import Text.Printf (printf)

evalQuery :: Query -> Json -> Either String [Json]
-- Identity
evalQuery Identity json' = Right [json']
-- Iterator
evalQuery Iterator (Object list' _) = Right (map snd list')
evalQuery Iterator (Array list') = Right list'
evalQuery Iterator json' = Left (printf "Cannot iterate over %s" (name json'))
-- Object Index
evalQuery (ObjectIndex key' _) (Object _ map') =
  case map' !? key' of
    Nothing -> Right [Null]
    Just value -> Right [value]
evalQuery (ObjectIndex _ isOptional') json' =
  if isOptional'
    then Right []
    else Left (printf "Cannot index %s with a string key" (name json'))
-- Pipe
evalQuery (Pipe query1 query2) json' = do
  result1 <- evalQuery query1 json'
  let (lefts, rights) = partitionEithers $ map (evalQuery query2) result1
  case lefts of
    (err : _) -> Left err
    [] -> Right (concat rights)
-- Array Index
evalQuery (ArrayIndex index' _) (Array list')
  | index' < 0 =
      if abs index' <= length list'
        then Right [list' !! (length list' + index')]
        else Right [Null]
  | index' > length list' = Right [Null]
  | otherwise = Right [list' !! index']
evalQuery (ArrayIndex _ isOptional') json' =
  if isOptional'
    then Right []
    else Left (printf "Cannot index %s with a number" (name json'))
-- Comma
evalQuery (Comma query1 query2) json' = do
  results1 <- evalQuery query1 json'
  results2 <- evalQuery query2 json'
  pure (results1 ++ results2)
-- Slice
evalQuery (Slice mStart mEnd) json' = case json' of
  Array xs -> Right [Array (sliceList xs)]
  Str text' -> Right [Str (sliceText text')]
  _ -> Left (printf "Cannot slice %s" (name json'))
  where
    len = case json' of
      Array xs -> length xs
      Str text' -> Text.length text'
      _ -> 0

    start' = normalizeStart mStart len
    end' = normalizeEnd mEnd len

    sliceList :: [a] -> [a]
    sliceList xs = getRange xs start' end'

    sliceText :: Text -> Text
    sliceText t = Text.pack (getRange (Text.unpack t) start' end')

-- | Extract the range of a list
getRange :: [a] -> Int -> Int -> [a]
getRange xs a b
  | b <= a = []
  | otherwise = take (b - a) (drop a xs)

-- | Normalize a number for an index at the start (useful for negative indices)
normalizeStart :: Maybe Int -> Int -> Int
normalizeStart Nothing _ = 0
normalizeStart (Just i) len = clamp len (if i < 0 then len + i else i)

-- | Normalize a number for an index at the end (useful for negative indices)
normalizeEnd :: Maybe Int -> Int -> Int
normalizeEnd Nothing len = len
normalizeEnd (Just i) len = clamp len (if i < 0 then len + i else i)

-- | Clamp a number between 0 and the length of a list
clamp :: Int -> Int -> Int
clamp len i = max 0 (min len i)
