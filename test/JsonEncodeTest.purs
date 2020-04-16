module Test.JsonEncodeTest (test) where

import Prelude

import AST (Expr(..))
import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Text.Parsing.Parser.Pos (initialPos)

test :: Effect Unit
test = do
    launchAff_ $ runSpec [consoleReporter] do
        describe "convert AST to json strings" do
            it "symbol" do
                let m = "{\"name\":\"symbol\",\"value\":\"hello\"}"
                (show $ Symbol initialPos "hello") `shouldEqual` m

            it "text" do
                let m = "{\"name\":\"text\",\"value\":\"hello \\\"world\\\"!\"}"
                (show $ Text initialPos "hello \"world\"!") `shouldEqual` m

            it "natural number" do
                let m = "{\"name\":\"nat\",\"value\":\"123\"}"
                (show $ Nat initialPos 123) `shouldEqual` m

            it "pair of primitive values" do
                let m = "{\"name\":\"pair\",\"value\":[{\"name\":\"symbol\",\"value\":\"Element1\"},{\"name\":\"text\",\"value\":\"element 2\"}]}"
                (show $ Pair initialPos [Symbol initialPos "Element1", Text initialPos "element 2"]) `shouldEqual` m
        
            it "set of primitive values" do
                let m = "{\"name\":\"set\",\"value\":[{\"name\":\"nat\",\"value\":\"123\"},{\"name\":\"nat\",\"value\":\"456\"},{\"name\":\"nat\",\"value\":\"789\"}]}"
                (show $ Set initialPos [Nat initialPos 123, Nat initialPos 456, Nat initialPos 789]) `shouldEqual` m

            it "lambda expression" do
                let m = "{\"name\":\"fun\",\"value\":[{\"name\":\"symbol\",\"value\":\"x\"},{\"name\":\"symbol\",\"value\":\"y\"}]}"
                (show $ Fun initialPos [Symbol initialPos "x", Symbol initialPos "y"]) `shouldEqual` m

            it "applying function" do
                let m = "{\"name\":\"apply\",\"value\":[{\"name\":\"symbol\",\"value\":\"add\"},{\"name\":\"nat\",\"value\":\"1\"},{\"name\":\"nat\",\"value\":\"2\"}]}"
                (show $ Apply initialPos [Symbol initialPos "add", Nat initialPos 1, Nat initialPos 2]) `shouldEqual` m
