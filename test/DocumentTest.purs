module Test.DocumentTest (test) where

import Prelude

import AST (Document(..), Annotation(..), Expr(..), Path(..), PathElement(..), Spec(..), Symbol(..), Term(..))
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Set as S
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parser as P
import Test.Test (phase, shouldBe)
import Text.Parsing.Parser (runParser)

test :: Effect Unit
test = do
    phase "document" $ do
        -------------------------------------------------------
        document_01 <- readTextFile UTF8 "./test/document_01.glz"
        shouldBe "simple"  
            ( runParser document_01 P.document )
            $ Document
                { name : "Document_1"
                , terms : fromFoldable 
                    [ Term {name: "post", desc: "ユーザの投稿", synonyms: ["投稿", "ポスト"]}
                    ]
                , specs : fromFoldable  
                    [  Api 
                        { annotation: Nothing
                        , method: "get"
                        , path: Path (fromFoldable [PStatic "users" , PAssignable (Symbol "id"), PStatic "posts"])
                        , query: VSet (S.fromFoldable [])
                        , pre: Lambda (Symbol "x") (VSet (S.fromFoldable [VBool true]))
                        , post: Lambda (Symbol "x") (VSet ( S.fromFoldable 
                                     [ BinOp 
                                        "=>" 
                                        (Call "note" (fromFoldable [VText "hoge"]))
                                        (Call "note" (fromFoldable [VText "fuga"]))
                                    ]))
                        }
                    , Api 
                        { annotation: Just $ Annotation "add post\nmore description"
                        , method: "post"
                        , path: Path (fromFoldable [PStatic "users" , PAssignable (Symbol "id"), PStatic "posts"])
                        , query: VSet (S.fromFoldable [])
                        , pre: Lambda (Symbol "x") (VSet (S.fromFoldable [VBool true]))
                        , post: Lambda 
                            (Symbol "x") 
                            (Call "satisfy" $
                                fromFoldable 
                                    [VSet (S.fromFoldable 
                                        [ BinOp "=>" 
                                            (Call "note" (fromFoldable [VText "hoge"]))
                                            (Call "note" (fromFoldable [VText "fuga"]))
                                        ])
                                    ]
                            )
                        }
                    ]
                }


        
