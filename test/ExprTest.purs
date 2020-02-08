module Test.ExprTest (test) where

import Prelude

import AST (Expr(..))
import Data.List (fromFoldable)
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

        -------------------------------------------------------
        shouldBe "=" 
            ( runParser "note \"hoge\" = note \"fuga\"" P.expr)
            $ BinOp 
                "=" 
                (Call "note" (fromFoldable [VText "hoge"]))
                (Call "note" (fromFoldable [VText "fuga"]))

        -------------------------------------------------------
        shouldBe "is" 
            ( runParser "note \"hoge\" is \"string\"" P.expr)
            $ BinOp 
                "is" 
                (Call "note" (fromFoldable [VText "hoge"]))
                (VText "string")


    
