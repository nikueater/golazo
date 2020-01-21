module Test.ValueTest (test) where

import Prelude

import AST (Expr(..), Symbol(..))
import Data.List (fromFoldable)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Parser as P
import Test.Test (phase, shouldBe, shouldFail)
import Text.Parsing.Parser (runParser)

test :: Effect Unit
test = do
    phase "value bool" $ do
        -------------------------------------------------------
        shouldBe "true" 
            ( runParser "true" P.value)
            $ VBool true

        -------------------------------------------------------
        shouldBe "false"
            ( runParser "false" P.value)
            $ VBool false

    phase "value text" $ do
        -------------------------------------------------------
        shouldBe "text"
            ( runParser "\"Hello \\\"World!!\\\"\"" P.value)
            $ VText "Hello \"World!!\""

    phase "natural" $ do
        -------------------------------------------------------
        shouldBe "ok" 
            ( runParser "45" P.value )
            $ VNat 45

    phase "pair" $ do
        -------------------------------------------------------
        shouldBe "bool" 
            ( runParser "nameA: true" P.value )
            $ VPair (Tuple (Symbol "nameA") (VBool true))
        -------------------------------------------------------
        shouldBe "string" 
            ( runParser "nameB:\"this is a test\"" P.value )
            $ VPair (Tuple (Symbol "nameB") (VText "this is a test"))

    phase "set" $ do
        -------------------------------------------------------
        shouldBe "bools" 
            ( runParser "{true, true, false}" P.value )
            $ VSet (Set.fromFoldable [VBool true, VBool false])

        -------------------------------------------------------
        shouldBe "pairs" 
            ( runParser "{hoge: \"hello\", fuga: true}" P.value )
            $ VSet (Set.fromFoldable 
                    [ VPair (Tuple (Symbol "hoge") (VText "hello"))
                    , VPair (Tuple (Symbol "fuga") (VBool true))
                    ])

        -------------------------------------------------------
        shouldBe "nested set" 
            ( runParser 
                """{ hoge: "hello"
                   , fuga: { piyo: "world" }
                   }
                """ P.value )
            $ VSet ( Set.fromFoldable 
                        [ VPair (Tuple (Symbol "hoge") (VText "hello"))
                        , VPair 
                            (Tuple 
                                (Symbol "fuga") 
                                (VSet 
                                    (Set.fromFoldable 
                                        [ VPair (Tuple (Symbol "piyo") (VText "world")) 
                                        ]
                                    )
                                )
                            )
                        ]
                    )

    phase "call" $ do
        -------------------------------------------------------
        shouldBe "single arity" 
            ( runParser "note \"hello world\"" P.value )
            $ Call "note" (fromFoldable [VText "hello world"])

        -------------------------------------------------------
        shouldBe "multiple arities" 
            ( runParser "stringIsBool \"true\" false" P.value )
            $ Call "stringIsBool" (fromFoldable [VText "true", VBool false])

        -------------------------------------------------------
        shouldBe "no arity" 
            ( runParser "undefined" P.value )
            $ Call "undefined" (fromFoldable [])



