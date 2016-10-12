module XAlg.Infra.Rewrite (
-- ** Matching
  match,

-- ** Substitution
  subCtx,
  
-- ** Rewriting
  rewrite,
  rewrite',
  rewriteRec
) where

import XAlg.Foundation.Recursion
import XAlg.Foundation.Core

import Data.Map            (Map, intersectionWith, singleton, empty, union, lookup)
import Control.Monad       ((<=<))
import Data.Maybe          (fromMaybe)
import Prelude      hiding (lookup)


-- Combining functions. Revisit for performance.

-- | Verify that two maps do not contain different values for the same key.
noCollisions :: (Ord k, Eq a) => Map k a -> Map k a -> Bool
noCollisions m1 m2 = and $ intersectionWith (==) m1 m2

-- | Combine (union) two maps, failing with `Nothing` if there are key collisions
--   as detected by `noCollisions`.
combine :: (Ord k, Eq a) => Map k a -> Map k a -> Maybe (Map k a)
combine m1 m2 | noCollisions m1 m2 = pure (union m1 m2)
              | otherwise          = Nothing

-- | Version of `Combine` that takes a `Maybe` value in the second position.
combine' :: (Ord k, Eq a) => Map k a -> Maybe (Map k a) -> Maybe (Map k a)
combine' m = (>>= combine m)

-- | Fold together a set of Maybe Maps combining with `combine'`, and failing if any
--   of them are `Nothing`.
combineAll :: (Ord k, Eq a) => [Maybe (Map k a)] -> Maybe (Map k a)
combineAll = foldr combine' (Just empty) <=< sequenceA


-- | Attempt to match the second expression argument to the form of the first.
--   Return a Map containing the subexpression bindings needed to be substitued into the
--   first argument in order to precisely yield the second. Nothing implies match failure.
match :: SymbolicLang x s => x -> x -> Maybe (Env x)
match (Literal x)  (Literal y) = if x == y then pure empty else Nothing
match (Variable x) y           = pure (singleton x y)
match x y | x `nodeEquality` y = combineAll $ zipWith match (children x) (children y)
          | otherwise          = Nothing


substituteCtx :: XAlgLang x f s => Env x -> Algebra f x
substituteCtx m (Variable_ v) = fromMaybe (variable v) (lookup v m)
substituteCtx m x             = foldFix x

-- | Given a mapping of names to expressions, take an expression and substitute in any variables
--   occurring in the map with the corresponding value.
subCtx :: XAlgLang x f s => Env x -> x -> x
subCtx m = catamap $ substituteCtx m

-- | Take an equation, and an expression. If the expression can `match` the left side of the
--   equation, then rewrite it by transforming it to the form of the right side.
rewrite :: XAlgLang x f s => Equation x -> x -> Maybe x
rewrite eq x = flip subCtx (rhs eq) <$> match (lhs eq) x

-- | Same as `rewrite`, only returns the expression unchanged on failure instead of `Nothing`.
rewrite' :: XAlgLang x f s => Equation x -> x -> x
rewrite' eq x = fromMaybe x $ rewrite eq x

-- | Recursively apply `rewrite'` throughout an expression.
rewriteRec :: XAlgLang x f s => Equation x -> x -> x
rewriteRec eq = recursively (rewrite' eq)
