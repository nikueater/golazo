module Test.Main where

import Prelude

import Effect (Effect)
import Test.TermTest as Term
import Test.DocumentTest as Document
import Test.AnnotationTest as Annotation
import Test.PathTest as Path
import Test.ExprTest as Expr
import Test.ValueTest as Value
import Test.SpecTest as Spec

main :: Effect Unit
main = do
    Document.test
    Annotation.test
    Term.test
    Expr.test
    Value.test
    Path.test
    Spec.test
