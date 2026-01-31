module Query.AST
  ( Query (..),
  )
where

data Query
  = Identity
  deriving (Show, Eq)
