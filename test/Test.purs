module Test.Test (shouldBe, phase, shouldFail) where

import Prelude

import Control.Monad.Except (throwError)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (log, error)
import Effect.Exception as Exp
import Text.Parsing.Parser (ParseError)

data Result = Success String | Failed String


test :: forall a. Eq a => Show a => String -> (Either ParseError a) -> a -> Either String String
test msg value expected =
    case value of
        Right v ->
            if v == expected
                then Right ("passed: " <> (show v))
                else Left ("failed: " <> "\n  expected: \t" <> (show expected) <> "\n  actual: \t" <> (show v))
        Left e ->
            Left $ show e
    
shouldBe :: forall a. Eq a => Show a => String -> (Either ParseError a) -> a -> Effect Unit
shouldBe msg value expected = do
    case test msg value expected of
        Right v ->
            log $ text "⭕️ " v
        Left v -> do
            _ <- error $ text "❌ " v
            throwError $ Exp.error msg
    where
          text :: String -> String -> String
          text pre result = "  " <> pre <> "\"" <> msg <> "\"... " <> result 

shouldFail :: forall a. Eq a => Show a =>  String -> (Either ParseError a) -> Effect Unit
shouldFail msg value = do
    case value of
         Right v -> do
             _ <- error $ text "❌" (show v)
             throwError $ Exp.error msg
         Left v ->
             log $ text "⭕️" (show v)
     where
          text :: String -> String -> String
          text pre result = "  " <> pre <> "\"" <> msg <> "\"... " <> result 


phase :: String -> Effect Unit -> Effect Unit
phase msg tests = do
    log $ "[" <> msg <> "]"
    tests
    log "\n"


