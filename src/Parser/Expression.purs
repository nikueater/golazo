module Parser.Expression where 

import Prelude

import AST (Expr(..))
import AST as AST
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array as Array
import Language (lexer)
import Text.Parsing.Parser (Parser, position)
import Text.Parsing.Parser.Combinators (choice, notFollowedBy, sepBy1, try)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.String (skipSpaces, string, whiteSpace)


expression :: Parser String Expr
expression = do
        try keyValue
        <|> try binary
        <|> term

    where
        keyValue :: Parser String Expr
        keyValue = do
            p <- position
            k <- lexer.identifier
            string ":" *> skipSpaces
            v <- expression
            pure (AST.keyValue p k v)

        binary ::  Parser String Expr
        binary = fix $ \_ ->
            buildExprParser 
                [ [Infix (operator ["."]) AssocLeft]
                , [Infix (operator ["*", "/","%"]) AssocLeft]
                , [Infix (operator ["+", "-"]) AssocLeft]
                , [Infix (operator [">", "<", ">=", "<="]) AssocLeft]
                , [Infix (operatorEq) AssocLeft]
                , [Infix (operator ["in"]) AssocRight]
                , [Infix (operator ["&&"]) AssocLeft]
                , [Infix (operator ["||"]) AssocLeft]
                , [Infix (operator ["&"]) AssocLeft]
                , [Infix (operator ["=>"]) AssocLeft]
                ] term

        operator :: Array String -> Parser String (Expr -> Expr -> Expr)
        operator ops = do 
            p <- position
            o <- try (choice $ map string ops)<* skipSpaces
            pure (AST.binary p o)

        operatorEq :: Parser String (Expr -> Expr -> Expr)
        operatorEq = do 
            p <- position
            o <- try $ choice 
                [ string "!="
                , string "=" <* notFollowedBy (string ">")
                ] 
            skipSpaces
            pure (AST.binary p o)

         

term :: Parser String Expr
term =  
    fix $ \_ -> lexer.parens expression
    <|> try apply
    <|> try lambda
    <|> value
    where
        lambda :: Parser String Expr
        lambda = do
           fix $ \_ -> do
                p <- position
                lexer.symbol "\\" *> skipSpaces
                x <- do 
                    pos <- position
                    name <- string "_" <|> lexer.identifier
                    pure (Symbol pos name)
                skipSpaces
                lexer.symbol "->" *> skipSpaces
                e <- expression
                pure (Fun p [x, e])

        apply :: Parser String Expr
        apply = do
            p <- position
            n <- lexer.identifier
            ns <- term `sepBy1` whiteSpace
            skipSpaces
            pure (AST.apply p n $ Array.fromFoldable ns)


factor :: Parser String Expr
factor =  
    fix $ \_ -> lexer.parens expression
    <|> value


value :: Parser String Expr
value = do
    try boolean
    <|> try text
    <|> try natural
    <|> try set
    <|> ident
    where
        boolean :: Parser String Expr
        boolean = do
           p <- position
           b <- do 
               lexer.symbol "true" *> pure true
               <|> lexer.symbol "false" *> pure false
           pure (AST.bool p b)

        text :: Parser String Expr
        text = do
           p <- position
           t <- lexer.stringLiteral
           pure (Text p t)

        natural :: Parser String Expr
        natural = do
           p <- position
           n <- lexer.natural
           pure (Nat p n)

        set :: Parser String Expr
        set = do
            fix $ \_ -> do
                p <- position
                xs <- lexer.braces (lexer.commaSep expression)
                pure (Set p $ Array.fromFoldable xs)

        ident :: Parser String Expr
        ident = do
            p <- position
            n <- lexer.identifier
            pure (Symbol p n)


