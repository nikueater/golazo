module Test.AnnotationTest (test) where


import Prelude
import Test.Test (phase, shouldBe)
import Effect (Effect)
import Text.Parsing.Parser (runParser)
import Parser as P
import AST (Annotation(..))


test :: Effect Unit
test =
    phase "annotation" $ do
        -------------------------------------------------------
        shouldBe "normal case"
            ( runParser 
                """# this is an annotation
                """ P.annotation
            ) 
            $ Annotation "this is an annotation"

        -------------------------------------------------------
        shouldBe "normal case without spaces at begining"
            ( runParser 
                """#this is an annotation
                """ 
                P.annotation
            ) 
            $ Annotation "this is an annotation"

        -------------------------------------------------------
        shouldBe "non alphabet"
            ( runParser 
                """# これはアノテーションです
                """ 
                P.annotation
            ) 
            $ Annotation "これはアノテーションです"
            


