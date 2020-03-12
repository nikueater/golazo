module Test.DocumentTest (test) where

import Prelude

import AST as A
import Effect (Effect)
import Parser as P
import Parser.Expression as E
import Test.Test (phase, pos, shouldBe)
import Text.Parsing.Parser (runParser)

test :: Effect Unit
test = do
    phase "document" $ do
        -------------------------------------------------------
        shouldBe "simple"  
            ( runParser "spec Test\n\n" P.document )
            $ A.document 
                { name: "Test"
                , terms: []
                , endpoints: []
                }

    phase "object" $ do
        -------------------------------------------------------
        shouldBe "key-value 1"  
            ( runParser "a : \"hoge\"" E.expression )
            $ A.keyValue pos "a" (A.Text pos "hoge")

        shouldBe "key-value 2"  
            ( runParser "{a: \"hoge\", b: 100}" E.expression )
            $ A.Set pos
                [ A.keyValue pos "a" (A.Text pos "hoge")
                , A.keyValue pos "b" (A.Nat pos 100)
                ]

        shouldBe "symbols"
            (runParser "{true, false, false}" E.expression)
             $ A.Set pos
                [ A.Symbol pos "true"
                , A.Symbol pos "false"
                , A.Symbol pos "false"
                ]   

    phase "binary operator" $ do
        -------------------------------------------------------
        shouldBe "binary"  
            ( runParser "10 + 2" E.expression )
            $ A.binary pos "+" (A.Nat pos 10) (A.Nat pos 2)

        shouldBe "binary with parens"  
            ( runParser "10 + (2 + 4)" E.expression )
            $ A.binary pos 
                "+" 
                (A.Nat pos 10) 
                (A.binary pos "+" (A.Nat pos 2) (A.Nat pos 4))

        shouldBe "in operator"
            ( runParser "x in String" E.expression )
            $ A.binary pos
                "in"
                (A.Symbol pos "x")
                (A.Symbol pos "String")

        shouldBe "imply 1"
            ( runParser "x = 10 => y = 5" E.expression )
            $ A.binary pos
                "=>"
                (A.binary pos "=" (A.Symbol pos "x") (A.Nat pos 10))
                (A.binary pos "=" (A.Symbol pos "y") (A.Nat pos 5))

        shouldBe "imply 2"
            ( runParser "note \"it's sunny\" => umbrella != true" E.expression )
            $ A.binary pos
                "=>"
                (A.apply pos "note" [A.Text pos "it's sunny"])
                (A.binary pos "!=" (A.Symbol pos "umbrella") (A.bool pos true))

        shouldBe "access member"
            ( runParser "hoge.fuga" E.expression )
            $ A.binary pos
                "."
                (A.Symbol pos "hoge")
                (A.Symbol pos "fuga")

    phase "apply" $ do
        -------------------------------------------------------
        shouldBe "simple"  
            ( runParser "add 1 2" $ E.expression)
            $ A.binary pos "add" (A.Nat pos 1) (A.Nat pos 2)

    {-
    phase "endpoint block" $ do
        -------------------------------------------------------
        shouldBe "parts"  
            ( runParser "query \\x -> \n satisfy x {x + 2 = 10}" $ PE.block "query" )
            $ A.binary pos "+" (A.Nat pos 10) (A.Nat pos 2)
    -}
        
