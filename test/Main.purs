module Test.Main where

import Prelude

import Effect (Effect)
import Test.DocumentTest as Document
import Test.JsonEncodeTest as JsonEncodeTest

main :: Effect Unit
main = do
    Document.test
    JsonEncodeTest.test
