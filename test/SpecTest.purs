module Test.SpecTest (test) where

import Prelude

import AST (Annotation(..), Expr(..), Path(..), PathElement(..), Spec(..), Symbol(..))
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Set as S
import Effect (Effect)
import Parser as P
import Test.Test (phase, shouldBe)
import Text.Parsing.Parser (runParser)

test :: Effect Unit
test = do
    phase "api" $ do
        -------------------------------------------------------
        shouldBe "simple"  
            ( runParser 
                """get /users/:id/posts 
                    query {}
                    pre \x ->
                        { true
                        }
                    post \x ->
                        { note "hoge" => note "fuga"
                        }
                """ 
                P.api
            )
            $ Api 
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

        -------------------------------------------------------
        shouldBe "with annotation"  
            ( runParser 
                """# remove all the posts an user made
                    delete /users/:id/posts 
                    query \x ->
                        { true
                        }
                    pre \x ->
                        { true
                        }
                    post \x ->
                        { note "hoge" => note "fuga"
                        }
                """ 
                P.api
            )
            $ Api 
                { annotation: Just (Annotation "remove all the posts an user made")
                , method: "delete"
                , path: Path (fromFoldable [PStatic "users" , PAssignable (Symbol "id"), PStatic "posts"])
                , query: Lambda (Symbol "x") (VSet (S.fromFoldable [VBool true]))
                , pre: Lambda (Symbol "x") (VSet (S.fromFoldable [VBool true]))
                , post: Lambda (Symbol "x") (VSet ( S.fromFoldable 
                             [ BinOp 
                                "=>" 
                                (Call "note" (fromFoldable [VText "hoge"]))
                                (Call "note" (fromFoldable [VText "fuga"]))
                            ]))
                }


