module XAlg.Infra.Logic where

import XAlg.Foundation.Recursion
import XAlg.Foundation.Core

import Data.Set  (Set, union, fromList, empty)
import qualified Data.Map as Map


-- Background: http://cstheory.stackexchange.com/questions/5696/how-do-tactics-work-in-proof-assistants

infixr 3 :=>

data Proposition x = Eqn (Equation x)
                   | And (Proposition x) (Proposition x)
                   | Or  (Proposition x) (Proposition x)
                   | Not (Proposition x)
                   | (:=>) (Proposition x) (Proposition x) -- Implies
                   | Void

infixl 5 /\
infixl 5 \/

(/\) = And
(\/) = Or

deriving instance Show x => Show (Proposition x)


data Axioms x = Axioms { axiomProps :: [Named (Proposition x)] }

deriving instance Show x => Show (Axioms x)



data Named a = Named String a deriving Functor

instance Show a => Show (Named a) where
  show (Named s x) = show s ++ " –– " ++ show x

instance Renderable a => Renderable (Named a) where
  render (Named s x) = show s ++ " –– " ++ render x


toPair :: Named a -> (String,a)
toPair (Named s x) = (s,x)

fromPair :: (String,a) -> Named a
fromPair (s,x) = Named s x

toMap :: [Named a] -> Map.Map String a
toMap = Map.fromList . map toPair




data System x = System
  { axioms :: Axioms x
  , postulates :: Axioms x
  , environ :: Set String
  }

assumptions :: System x -> [Named (Proposition x)]
assumptions sys = axiomProps (axioms sys) ++ axiomProps (postulates sys)

data ProofStep x = Rewrite (Equation x)

data Proof x = Proof
  { claim :: Proposition x
  , steps :: [ProofStep x] }
