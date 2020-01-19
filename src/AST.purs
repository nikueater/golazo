module AST where

import Control.Semigroupoid ((<<<))
import Data.Array (fromFoldable)
import Data.Eq (class Eq, eq)
import Data.Functor (map)
import Data.HeytingAlgebra ((&&))
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Monoid ((<>))
import Data.Ord (class Ord, compare)
import Data.Set (Set)
import Data.Set as Set
import Data.Show (class Show, show)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))



-- Symbol
newtype Symbol = Symbol String

instance eqSymbol :: Eq Symbol where
    eq (Symbol a) (Symbol b) = eq a b

instance showSymbol :: Show Symbol where
    show (Symbol a) = ":" <> a

instance ordSymbol :: Ord Symbol where
    compare (Symbol a) (Symbol b) = compare a b

-- SPEC
newtype Document = Document
    { name :: String
    , specs :: List Spec
    , terms :: List Term
    }

instance eqDocument :: Eq Document where
    eq (Document a) (Document b) = (eq a.name b.name) && (eq a.specs b.specs) && (eq a.terms b.terms)

instance showDocument :: Show Document where
    show (Document a) = show a


-- ANNOTATION
newtype Annotation = Annotation String   

instance eqAnnotaion :: Eq Annotation where
    eq (Annotation a) (Annotation b) = eq a b

instance showAnnotation :: Show Annotation where
    show (Annotation a) = "@" <> a


-- TERM
newtype Term = Term
    { name :: String
    , desc :: String
    , synonyms :: Array String
    }

instance eqTerm :: Eq Term where
    eq (Term a) (Term b) = eq a b 

instance showTerm :: Show Term where
    show (Term a) = "{ name = \"" <> a.name <> "\", desc = \"" <> a.desc <> "\", synonyms = \"" <> (joinWith "," a.synonyms) <> "\" }"

-- EXPRESSION

-- VALUE
data Expr
     = VBool Boolean
     | VText String
     | VPair (Tuple Symbol Expr)
     | VSet (Set Expr)
     | Call String (List Expr)
     | BinOp String Expr Expr 
     | Lambda Symbol Expr

instance eqExpr :: Eq Expr where
    eq (VBool a) (VBool b) = eq a b
    eq (VText a) (VText b) = eq a b
    eq (VPair a) (VPair b) = eq a b
    eq (VSet a) (VSet b) = eq a b
    eq (Call an aas) (Call bn bas) = (eq an bn) && (eq aas bas)
    eq (BinOp an al ar) (BinOp bn bl br) = (eq an bn) && (eq al bl) && (eq ar br)
    eq (Lambda av ae) (Lambda bv be) = (eq av bv) && (eq ae be) 
    eq _ _ = false

instance showExpr :: Show Expr where
    show (VBool a) = show a
    show (VText a) = "\"" <> a <> "\"" 
    show (VPair (Tuple k v)) = "(" <> show k <> ", " <> show v <> ")"
    show (VSet a) = "{" <> (joinWith ", " <<< fromFoldable <<< Set.map show) a <> "}"
    show (Call name args) = "{:call, :" <> name <> ", [" <> (joinWith ", " <<< fromFoldable <<< map show) args <> "]}"  
    show (BinOp op tl tr) = "{:call, :(" <> op <> "), [" <> (joinWith ", " <<< map show) [tl, tr] <> "]}"
    show (Lambda (Symbol v) e) = "λ" <> v <> "->" <> (show e)

derive instance ordExpr :: Ord Expr


-- PATH
data PathElement
    = PStatic String
    | PAssignable Symbol

instance eqPathElement :: Eq PathElement where
    eq (PStatic a) (PStatic b) = eq a b
    eq (PAssignable a) (PAssignable b) = eq a b
    eq _ _ = false

instance showPathElement :: Show PathElement where
    show (PStatic a) = a
    show (PAssignable a) = show a

newtype Path = Path (List PathElement)

derive instance eqPath :: Eq Path

instance showPath :: Show Path where
    show (Path a) = "/" <> (joinWith "/" <<< map show <<< fromFoldable) a


-- API
data Spec
    = Api 
        { annotation :: Maybe Annotation
        , method :: String
        , path :: Path
        , query :: Expr
        , pre :: Expr
        , post :: Expr
        }
    
derive instance eqSpec :: Eq Spec
instance showSpec :: Show Spec where
    show (Api a) = show a


