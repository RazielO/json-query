module Query.AST
  ( Query (..),
  )
where

import Data.Text (Text)

data Query
  = Identity
  | Iterator
  | ObjectIndex {key :: Text, isOptional :: Bool}
  | ArrayIndex {index :: Int, isOptional :: Bool}
  | Pipe {lhs :: Query, rhs :: Query}
  | Comma {lhs :: Query, rhs :: Query}
  | Slice {start :: Maybe Int, end :: Maybe Int}
  deriving (Show, Eq)
