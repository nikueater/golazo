module Parser.Endpoint where

import Prelude

import AST (Expr(..), keyValue)
import AST as AST
import Control.Alt ((<|>))
import Data.Array as Array
import Data.List (List(..))
import Data.List as List
import Language (lexer)
import Parser.Expression (expression)
import Parser.Expression as E
import Text.Parsing.Parser (Parser, position)
import Text.Parsing.Parser.String (char, skipSpaces, string)


endpoint :: Parser String Expr
endpoint = do
    pos1 <- position
    m <- eMethod
    p <- ePath
    pos2 <- position
    spec <- E.value
    pure $ Set pos1 [m, p, keyValue pos2 "spec" spec]
    where
        eMethod :: Parser String Expr
        eMethod = do
            p <- position
            m <- do
                string "get"
                <|> string "post"
                <|> string "put"
                <|> string "delete"
            skipSpaces
            pure $ keyValue p "method" (Symbol p m)
        
        ePath :: Parser String Expr
        ePath = do
           pos <- position
           p <- path
           pure $ keyValue pos "path" p

        

block :: String -> Parser String Expr
block name = do
    pos <- position
    q <- skipSpaces *> string name *> skipSpaces *> E.expression
    skipSpaces <* string ";"
    pure $ keyValue pos name q

path :: Parser String Expr
path = do
    p <- position
    h <- element
    t <- List.many element
    pure $ AST.sequential p $ Array.fromFoldable $ Cons h t
    where 
        element :: Parser String Expr
        element = do
            _ <- lexer.symbol "/"
            x <- do static <|> param
            pure x
            
        param :: Parser String Expr
        param = do
            p <- position
            char ':' *> skipSpaces
            name <- lexer.identifier
            pure $ Symbol p name

        static :: Parser String Expr
        static = do
            p <- position
            name <- lexer.identifier
            pure $ Text p name
 
