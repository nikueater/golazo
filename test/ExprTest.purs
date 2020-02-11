module Test.ExprTest (test) where

import Prelude

import AST (Expr(..), Symbol(..))
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

        -------------------------------------------------------
        shouldBe "dealing with objects" 
            ( runParser "x.foo => y.bar" P.expr)
            $ BinOp 
                "=>" 
                (Call "@get" (fromFoldable 
                    [ Refer (Symbol "x")
                    , VList $ fromFoldable 
                        [ VSymbol (Symbol "foo")
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

    
