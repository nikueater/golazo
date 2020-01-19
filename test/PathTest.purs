module Test.PathTest (test) where

import Prelude

import AST (Path(..), PathElement(..), Symbol(..))
import Data.List (fromFoldable)
import Effect (Effect)
import Parser as P
import Test.Test (phase, shouldBe)
import Text.Parsing.Parser (runParser)

test :: Effect Unit
test = do
    phase "path" $ do
        -------------------------------------------------------
        shouldBe "single static" 
            ( runParser "/root" P.path)
            $ Path (fromFoldable [PStatic "root"])
        
        -------------------------------------------------------
        shouldBe "multiple statics" 
            ( runParser "/root/child" P.path)
            $ Path (fromFoldable [PStatic "root", PStatic "child"])

        -------------------------------------------------------
        shouldBe "with assignables" 
            ( runParser "/root/:p1/child/:p2" P.path)
            $ Path (fromFoldable [PStatic "root", PAssignable (Symbol "p1"), PStatic "child", PAssignable (Symbol "p2")])



 

