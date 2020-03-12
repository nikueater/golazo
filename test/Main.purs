module Test.Main where

import Prelude

import Effect (Effect)
import Test.DocumentTest as Document

main :: Effect Unit
main = do
    Document.test
