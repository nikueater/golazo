module Parser where


import Prelude

import AST as A
import Data.Array (fromFoldable, many)
import Language (lexer)
import Parser.Endpoint as Endpoint
import Parser.Glossary as Glossary
import Text.Parsing.Parser (Parser, position)
import Text.Parsing.Parser.Combinators (manyTill)
import Text.Parsing.Parser.String (eof, skipSpaces)

document :: Parser String A.Expr
document = do
    lexer.reserved "spec"
    p <- position
    name <- lexer.identifier
    skipSpaces
    terms <- many Glossary.term
    skipSpaces 
    endpoints <- manyTill Endpoint.endpoint eof
    pure $ A.document
        { name : name
        , terms : terms
        , endpoints : fromFoldable endpoints
        }

