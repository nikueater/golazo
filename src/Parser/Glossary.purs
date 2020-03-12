module Parser.Glossary (term) where

import Prelude

import AST (Expr(..), keyValue)
import Data.Array (fromFoldable)
import Data.List (List)
import Language (lexer)
import Text.Parsing.Parser (Parser, position)
import Text.Parsing.Parser.Pos (initialPos)
import Text.Parsing.Parser.String (skipSpaces, string)


-- | parse term definition

term :: Parser String Expr
term = do
    p <- position
    string "term" *> skipSpaces
    name <- lexer.stringLiteral <* skipSpaces
    string "description"  *> skipSpaces
    desc <- lexer.stringLiteral
    string "synonyms" *> skipSpaces
    synonyms <- lexer.braces (lexer.commaSep lexer.stringLiteral)
    pure $ keyValue p name $ 
        Set initialPos 
        [ keyValue initialPos "description" (Text initialPos desc)
        , keyValue  initialPos "synonyms" $ synonymSet synonyms
        ]
    where
          synonymSet :: List String -> Expr
          synonymSet xs = 
              Set initialPos $ fromFoldable $ map (Text initialPos) xs

