module AST where

import Prelude

import Data.Array as Array
import Data.List (List(..))
import Data.List as List
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser.Pos (Position, initialPos)


keyValue :: Position -> String -> Expr -> Expr
keyValue p k v = 
    Pair p [Symbol initialPos k, v]

document :: {name :: String, terms :: Array Expr, endpoints :: Array Expr} -> Expr
document {name, terms, endpoints} =
    Pair initialPos
        [ Symbol initialPos name
        , keyValue initialPos "glossary" (Set initialPos terms)
        , keyValue initialPos "endpoints" (Set initialPos endpoints)
        ]

binary :: Position -> String -> Expr -> Expr -> Expr
binary pos op lt rt =
    Apply pos
        [ Symbol pos op
        , sequential pos [lt, rt]
        ]

apply :: Position -> String -> Array Expr -> Expr
apply pos fun args =
    Apply pos 
        [ Symbol pos fun
        , sequential pos args
        ]

-- | Converts an array to a set of pairs of index and value
sequential :: Position -> Array Expr -> Expr
sequential pos es =
    Set pos $ indexed 0 (List.fromFoldable es) Nil
    where
        indexed :: Int -> List Expr -> List Expr -> Array Expr
        indexed _ Nil ys = Array.fromFoldable ys
        indexed i (Cons x xs) ys = indexed (i + 1) xs (append i x ys)

        append :: Int -> Expr -> List Expr -> List Expr
        append i x xs = 
            List.snoc xs (Pair pos [Nat pos i, x])

-- | Converts a boolean value to a symbol
bool :: Position -> Boolean -> Expr
bool p true = Symbol p "true"
bool p false = Symbol p "false"




data Expr
    = Symbol Position String
    | Text Position String
    | Nat Position Int
    | Pair Position (Array Expr)
    | Set Position (Array Expr)
    | Fun Position (Array Expr)
    | Apply Position (Array Expr)

instance showExpr :: Show Expr where
    show (Symbol _ s) = showValue "symbol" s
    show (Text _ s) = showValue "text" s 
    show (Nat _ n) = showValue "nat" (show n)
    show (Pair _ xs) = showPair xs
    show (Set _ xs) = showSet xs
    show (Fun _ xs) = showFun xs
    show (Apply _ xs) = showApply xs

instance eqExpr :: Eq Expr where
    eq (Symbol _ a) (Symbol _ b) = a == b
    eq (Text _ a) (Text _ b) = a == b
    eq (Nat _ a) (Nat _ b) = a == b
    eq (Pair _ a) (Pair _ b) = a == b
    eq (Set _ a) (Set _ b) = a == b
    eq (Fun _ a) (Fun _ b) = a == b
    eq (Apply _ a) (Apply _ b) = a == b
    eq _ _ = false



showValue :: String -> String -> String
showValue name value = 
    node 
        [ Tuple "name" (show name)
        , Tuple "value" (show value) 
        ] 

showPair :: Array Expr -> String
showPair xs =
    node
        [ Tuple "name" (show "pair")
        , Tuple "value" $ arrayString xs
        ]

showSet :: Array Expr -> String
showSet xs =
    node 
        [ Tuple "name" (show "set")
        , Tuple "value" $ arrayString xs
        ]

showFun :: Array Expr -> String 
showFun xs =
    node
        [ Tuple "name" (show "fun")
        , Tuple "value" $ arrayString xs
        ]

showApply :: Array Expr -> String 
showApply xs =
    node
        [ Tuple "name" (show "apply")
        , Tuple "value" $ arrayString xs
        ]

arrayString :: Array Expr -> String
arrayString xs = 
    "[" <> values xs <> "]"
    where
          values = joinWith "," <<< map show

node :: Array (Tuple String String) -> String
node xs =  "{" <> properties xs <>  "}"
    where 
          kv :: Tuple String String -> String
          kv (Tuple k v) =  (string k) <> ":" <> v

          properties :: Array (Tuple String String) -> String
          properties = joinWith "," <<< map kv

          string :: String -> String
          string s = "\"" <> s <> "\""
