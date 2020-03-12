module Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parser (document)
import Text.Parsing.Parser (runParser)


main :: String -> Effect Unit
main file =  do
    x <- readTextFile UTF8 file
    log $ exec x
                
    where 
        exec :: String -> String
        exec code = 
            case runParser code document of
                    Left e -> show e
                    Right s -> show s
