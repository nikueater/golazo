module Parser where

import Prelude

import AST (Annotation(..), Expr(..), Path(..), PathElement(..), Document(..), Symbol(..), Term(..), Spec(..))
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (fold, fromFoldable)
import Data.List (List(..), many, toUnfoldable)
import Data.List as List
import Data.Set as Set
import Data.String (joinWith, trim)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Language (lexer)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (many1Till, manyTill, notFollowedBy, optionMaybe, try)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.String (anyChar, char, eof, skipSpaces, string)

newline :: Parser String Unit
newline = do
    (string "\n" *> pure unit)
    <|> (string "\r\n" *> pure unit) 
    <|> eof


document :: Parser String Document
document = do
    lexer.reserved "spec"
    name <- lexer.identifier
    skipSpaces
    terms <- many term
    skipSpaces
    specs <- many1Till (api <* skipSpaces) eof
    pure $ Document
        { name: name
        , terms: terms 
        , specs: specs
        }


annotation :: Parser String Annotation
annotation = do
    h <- line
    t <- many line
    pure $ Annotation $ (joinWith "\n" <<< toUnfoldable) (Cons h t)
    where
        line :: Parser String String
        line = do
            char '#' *> (manyTill anyChar newline >>= \x -> pure (toString x))

        toString :: List Char -> String
        toString = 
            toUnfoldable >>> fromCharArray >>> trim


term :: Parser String Term
term = do
    string "term" *> skipSpaces
    name <- lexer.stringLiteral <* skipSpaces
    string "description" *> skipSpaces
    desc <- lexer.stringLiteral <* skipSpaces
    string "synonyms" *> skipSpaces
    synonyms <- lexer.braces (lexer.commaSep lexer.stringLiteral) >>= fromFoldable >>> pure

    pure $ Term {name: name, desc: desc, synonyms: synonyms}


expr :: Parser String Expr
expr = do
    try binop
    <|> exprTerm
    where
        binop :: Parser String Expr
        binop = fix $ \_ ->
            buildExprParser
                [   [ Infix ops AssocRight
                    ]
                ,   [ Infix (op2 "=>") AssocRight
                    ]
                ]
                exprTerm

        op2 :: String -> Parser String (Expr -> Expr -> Expr)
        op2 x = (string x >>= pure <$> \y -> BinOp y) <* skipSpaces 

        ops :: Parser String (Expr -> Expr -> Expr)
        ops = 
            (
            ( try (string "=" <* notFollowedBy (string ">"))
                <|> string "!=" 
                <|> string "::" 
                <|> string ">"
                <|> string "<"
                <|> string ">="
                <|> string "<="
                <|> string "is"
                <|> string "&"
            ) >>= pure <$> \x -> BinOp x) <* skipSpaces
        

exprTerm :: Parser String Expr
exprTerm = do
    try call
    <|> value
    where
        call :: Parser String Expr
        call = fix $ \_ -> do
            name <- lexer.identifier
            skipSpaces
            argh <- value
            argt <- many value
            pure $ Call name (Cons argh argt)


value :: Parser String Expr
value = do
    fix $ \_ -> lexer.parens expr
    <|> try member
    <|> lambda
    <|> boolean
    <|> string
    <|> number
    <|> try pair
    <|> set
    <|> refer
    
    where
        boolean :: Parser String Expr
        boolean = do
            lexer.symbol "true" *> pure (VBool true)
            <|> lexer.symbol "false" *> pure (VBool false)

        string :: Parser String Expr
        string = do
           lexer.stringLiteral >>= \s -> pure (VText s)

        number :: Parser String Expr
        number = do
           lexer.natural >>= \n -> pure (VNat n)

        pair :: Parser String Expr
        pair = do
            k <- lexer.identifier >>= \x -> pure (Symbol x)
            char ':' *> skipSpaces
            v <- expr
            pure $ VPair (Tuple k v)

        set :: Parser String Expr
        set = fix $ \_ -> do
            xs <- lexer.braces (lexer.commaSep expr) >>= Set.fromFoldable >>> pure
            pure $ VSet xs

        lambda :: Parser String Expr
        lambda = fix $ \_ -> do
            lexer.symbol "\\" *> skipSpaces
            skipSpaces
            n <- lexer.identifier
            lexer.symbol "->"  *> skipSpaces
            e <- expr
            pure $ Lambda (Symbol n) e

        refer :: Parser String Expr
        refer = do
            s <- lexer.identifier >>= \x -> pure (Symbol x)
            pure $ Refer s

        call :: Parser String Expr
        call = fix $ \_ -> do
            name <- lexer.identifier
            skipSpaces
            argh <- value
            argt <- many value
            pure $ Call name (Cons argh argt)


        member :: Parser String Expr
        member = do
            parent <- lexer.identifier >>= \x -> pure (Refer $ Symbol x)
            _ <- lexer.symbol "."
            ch <- lexer.identifier >>= \x -> pure (VSymbol $ Symbol x)
            ct <- many (lexer.symbol "." *> lexer.identifier >>= \x -> pure (VSymbol $ Symbol x))
            (pure <<< Call "@get") $ List.fromFoldable [parent , VList (Cons ch ct) ]

path :: Parser String Path
path = do
    h <- element
    t <- many element
    pure $ Path (Cons h t)
    where 
        element :: Parser String PathElement
        element = do
            _ <- lexer.symbol "/"
            x <- do static <|> assignable
            pure x
            
        assignable :: Parser String PathElement
        assignable = do
             char ':' *> skipSpaces
             name <- lexer.identifier >>= \x -> pure (Symbol x)
             pure $ PAssignable name

        static :: Parser String PathElement
        static = do
            name <- lexer.identifier
            pure $ PStatic name
        

api :: Parser String Spec
api = do
    a <- optionMaybe (annotation <* skipSpaces)
    m <- method
    p <- path 
    query <- skipSpaces *> string "query" *> skipSpaces *> expr
    pre <- skipSpaces *> string "pre" *> skipSpaces *> expr
    post <- skipSpaces *> string "post" *> skipSpaces *> expr 
    pure $ Api 
        { annotation: a
        , method: m
        , path: p
        , query: query
        , pre: pre
        , post: post
        }
    where
        method = 
              ( string "get"
              <|> string "post"
              <|> string "put"
              <|> string "delete"
              ) <* skipSpaces
        
        

