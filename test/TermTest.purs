module Test.TermTest (test) where

import Prelude
import Test.Test (phase, shouldBe)
import Effect (Effect)
import Text.Parsing.Parser (runParser)
import Parser as P
import AST (Term(..))

test :: Effect Unit
test =
    phase "term" $ do
        -------------------------------------------------------
        shouldBe "normal" 
            ( runParser
            """term "submit" description "送信ボタン" synonyms { "a", "b", "c" }
                """
                P.term
            )
            $ Term {name: "submit", desc: "送信ボタン", synonyms: ["a", "b", "c"]}

        -------------------------------------------------------
        shouldBe "contains double quotes"
            ( runParser 
                """term "\"submit\"" 
                   description "\"送信\"ボタン"
                   synonyms {}
                """ 
                P.term
            ) 
            $ Term {name: "\"submit\"", desc: "\"送信\"ボタン", synonyms: []}
