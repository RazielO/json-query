module Query.Eval (evalQuery) where

import Json.AST (Json (..))
import Query.AST (Query (..))

evalQuery :: Query -> Json -> Either String [Json]
-- Identity
evalQuery Identity json' = Right [json']
