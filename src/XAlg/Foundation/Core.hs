{-# LANGUAGE Rank2Types #-}

-- | Core class hierarchy and other key functionality to support XAlg languages.
module XAlg.Foundation.Core (
  -- * Core class hierarchy
  -- ** Transcription
  Parsable (..),
  Renderable (..),
  Transcribe (..),

  -- ** Classes for *-kinded data type langauge representations
  RecursiveData (..),
  SymbolicLang (..),
  -- **** Associated data type
  EvalResult (..),

  -- ** Class for (* -> *)-kinded language rep
  ParaSymb (..),
  children_,
  numChildren,

  -- ** Master Language Class
  XAlgLang (..),

  -- * Other Core facilities
  -- ** Abstract pattern synomyns
  pattern Variable,
  pattern Variable_,
  pattern Literal,

  -- ** Data types

  Equation (..),
  Binding (..),
  Lang (..),

  -- ** For accessing environment

  exprPrism,
--  bindPrism,
  eqnPrism,
  exprs,
--  bindings,
  equations,

  -- ** Type synonyms

  Env,

  -- ** Subtyping class
  type (<:) (..)


) where

import XAlg.Foundation.Recursion

import Text.Megaparsec.String
import Data.Data           (Data)
import Data.Map            (Map)
import Data.Set     hiding (toList,map)
import Data.Foldable       (toList)
import Data.Maybe          (catMaybes, isJust)

class Parsable a where
  exprParser :: Parser a

class Renderable a where
  render :: a -> String -- richer format later

class (Parsable a, Renderable a) => Transcribe a



-- | Either the result of fully evaluating an expression, or an expression together with the set
--   of variables that must be plugged in to get the fully evaluated result.
data EvalResult x s = FunctionResult (Set String) x
                    | ValueResult s
                    deriving Functor

deriving instance (Show a, Show s) => Show (EvalResult a s)


class RecursiveData a where
  nodeEquality :: a -> a -> Bool

  children :: a -> [a]

  mapChildren :: (a -> a) -> a -> a

  recursively :: (a -> a) -> a -> a
  recursively f = f . mapChildren f

-- | A class the for symbolic languages that can represent subexpressions by named variables.
--   Must satisfy:
--
--   1) isVariable . variable = Just
--
--   2) isLiteral . literal = Just
--
--   3) evaluate . literal = ValueResult
--
--   The Eq constraint on the literal values is for the matching/rewrite machinery.
class (Eq a, Eq s, RecursiveData a) => SymbolicLang a s | a -> s where
  variable :: String -> a
  isVariable :: a -> Maybe String
  literal :: s -> a
  isLiteral :: a -> Maybe s
  evaluate :: a -> EvalResult a s

class Foldable f => ParaSymb f where
  variable_ :: String -> f a
  isVariable_ :: f a -> Maybe String

children_ :: ParaSymb f => f a -> [a]
children_ = toList

numChildren :: Foldable f => f a -> Int
numChildren = length . toList

  -- XXX redundant. Consolidate to the functorial data primarily?

pattern Variable :: SymbolicLang x s => String -> x
pattern Variable v <- (isVariable -> Just v)

pattern Variable_ :: XAlgLang x f s => String -> f a
pattern Variable_ v <- (isVariable_ -> Just v)

pattern Literal :: SymbolicLang x s => s -> x
pattern Literal l <- (isLiteral -> Just l)

-- pattern Literal_ :: XAlgLang x f s => String -> f a
-- pattern Literal_ v <- (isLiteral -> Just v)




class (Transcribe x, SymbolicLang x s, Fixable f x, ParaSymb f) =>
      XAlgLang x f s | x -> f s



data Binding x = Binding
  { boundName :: String
  , boundValue :: x  }

deriving instance Data x => Data (Binding x)
deriving instance Show x => Show (Binding x)
deriving instance Eq   x => Eq   (Binding x)
deriving instance Ord  x => Ord  (Binding x)

infixr 5 :=:

data Equation x = (:=:)
  { lhs :: x
  , rhs :: x }

deriving instance Data x => Data (Equation x)
deriving instance Show x => Show (Equation x)

instance Renderable x => Renderable (Equation x) where
  render (l :=: r) = render l ++ " = " ++ render r

sym :: Equation x -> Equation x
sym (l :=: r) = r :=: l




-------

data Lang x = EXPR x
            | EQUATION (Equation x)

deriving instance Data x => Data (Lang x)
deriving instance Show x => Show (Lang x)

instance Renderable x => Renderable (Lang x) where
  render (EXPR x) = render x
  render (EQUATION e) = render e


exprPrism :: Lang x -> Maybe x
exprPrism (EXPR x) = Just x
exprPrism _        = Nothing


eqnPrism :: Lang x -> Maybe (Equation x)
eqnPrism (EQUATION e) = Just e
eqnPrism _            = Nothing

isExpr, isEqn :: Lang x -> Bool
isExpr = isJust . exprPrism
isEqn  = isJust . eqnPrism

exprs :: Foldable t => t (Lang x) -> [x]
exprs = catMaybes . map exprPrism . toList

equations :: Foldable t => t (Lang x) -> [Equation x]
equations = catMaybes . map eqnPrism . toList




type Env x = Map String x







---- Not used yet, but maybe:


-- | Subtyping relation.
class a <: b where
  -- | Inclusion map.
  incl :: a -> b








{-
class f <<: g where
  inj :: f a -> g a

-- FlexibleInstances
instance f <<: (f :+: g) where
  inj = InL

instance g <<: (f :+: g) where
  inj = InR

data Metavar a = Metavar String

type Fragment f = f :+: Metavar

data Symb a = Vari String
            | Liter a
-}
