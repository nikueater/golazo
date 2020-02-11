module Test.ExprTest (test) where

import Prelude

import AST (Expr(..), Symbol(..))
import Data.List (fromFoldable)
import Data.Set (Set)
import Data.Set as Set
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

        -------------------------------------------------------
        shouldBe "in" 
            ( runParser "x in {1, 2, 3}" P.expr)
            $ BinOp 
                "in"
                (Refer (Symbol "x"))
                (VSet (Set.fromFoldable [VNat 1, VNat 2, VNat 3]))

        -------------------------------------------------------
        shouldBe "dealing with objects" 
            ( runParser "x.post.length > y.bar" P.expr)
            $ BinOp 
                ">" 
                (Call "@get" (fromFoldable 
                    [ Refer (Symbol "x")
                    , VList $ fromFoldable 
                        [ VSymbol (Symbol "post")
                        , VSymbol (Symbol "length")
                        ]
                    ]
                ))
                (Call "@get" (fromFoldable 
                    [ Refer (Symbol "y")
                    , VList $ fromFoldable
                        [ VSymbol (Symbol "bar")
                        ]
                    ]
                ))

        -------------------------------------------------------
        shouldBe "dealing with objects" 
            ( runParser "x.foo = null => y.bar != null" P.expr)
            $ BinOp 
                "=>" 
                (BinOp "="
                    (Call "@get" (fromFoldable 
                        [ Refer (Symbol "x")
                        , VList $ fromFoldable 
                            [ VSymbol (Symbol "foo")
                            ]
                        ]))
                    (Refer (Symbol "null"))
                )
                ( BinOp "!="
                    (Call "@get" (fromFoldable 
                        [ Refer (Symbol "y")
                        , VList $ fromFoldable
                            [ VSymbol (Symbol "bar")
                            ]
                        ]))
                    (Refer (Symbol "null"))
                )

    
