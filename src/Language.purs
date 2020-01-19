module Language (lexer) where

import Data.Char.Unicode (isAlpha)
import Data.Functor as Functor
import Data.Monoid ((<>))
import Data.String.CodeUnits as CodeUnits
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (oneOf, satisfy)
import Control.Alt ((<|>))
import Text.Parsing.Parser.Token as P




golazoStyle :: P.LanguageDef
golazoStyle = P.LanguageDef (P.unGenLanguageDef emptyDef)
    { caseSensitive = true
    , commentLine = ""
    , commentStart = ""
    , commentEnd = ""
    , identStart = satisfy isAlpha
    , identLetter = P.alphaNum <|> oneOf ['_']
    , nestedComments = false
    , opStart = oneOf opChars
    , opLetter = oneOf opChars
    , reservedNames = ["spec", "using", "term", "description", "synonyms"]
    , reservedOpNames = (Functor.map CodeUnits.singleton opChars) <> ["=>"]
    }
    where
          opChars = ['+', '-', '*', '/', '=', '>']

lexer :: P.TokenParser
lexer = P.makeTokenParser golazoStyle
