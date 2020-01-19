module Main where

import Prelude

import Data.Either (Either(..))
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parser (document)
import Text.Parsing.Parser (runParser)

foreign import argv :: Array String

args :: List String
args = List.drop 2 $ List.fromFoldable argv

main :: Effect Unit
main = 
    case List.head args of
        Just f -> do
            x <- readTextFile UTF8 f
            log $ exec x
                
        Nothing ->
            log "no file"
    where 
        exec :: String -> String
        exec code = 
            case runParser code document of
                    Left e -> show e
                    Right s -> show s
