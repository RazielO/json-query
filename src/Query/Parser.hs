module Query.Parser
  ( query,
    identity,
  )
where

import Parser (Parser, char, eof, lexeme, parseWhitespace)
import Query.AST (Query (..))

-- | Parser for a query
query :: Parser Query
query = parseWhitespace *> identity <* eof

-- | Identity parser
identity :: Parser Query
identity = Identity <$ lexeme (char '.')