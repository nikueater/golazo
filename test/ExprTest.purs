module Test.ExprTest (test) where

import Prelude

import AST (Expr(..), Symbol(..))
import Data.List (fromFoldable)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Parser as P
import Test.Test (phase, shouldBe)
import Text.Parsing.Parser (runParser)

test :: Effect Unit
test = do
    phase "binary expression" $ do
       -------------------------------------------------------
        shouldBe "=>" 
            ( runParser "true => false" P.expr)
            $ BinOp 
                "=>"
                (VBool true)
                (VBool false)
        -------------------------------------------------------
        shouldBe "=>" 
            ( runParser "note \"hoge\" => note \"fuga\"" P.expr)
            $ BinOp 
                "=>" 
                (Call "note" (fromFoldable [VText "hoge"]))
                (Call "note" (fromFoldable [VText "fuga"]))

    
