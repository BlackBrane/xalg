module XAlg.Foundation.Recursion (
  Fix (..),
  out,
  liftF2,
  Algebra,
  Coalgebra,
  cata,
  ana,
  topDown,
  bottomUp,
  Fixable (..),
  catamap,
  anamap,
  type (:+:) (..),
  sumFixL,
  sumFixR
) where

import Control.Arrow


-- http://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/
-- http://blog.sumtypeofway.com/recursion-schemes-part-iii-folds-in-context/

-- | Type-level fixed-point of a functor.
newtype Fix f = In (f (Fix f))

-- | Projection that unwraps a layer of a `Fix`.
out :: Fix f -> f (Fix f)
out (In fx) = fx


liftF2 :: (a -> b -> f (Fix f)) -> a -> b -> Fix f
liftF2 g x y = In $ g x y

-------

type Algebra f a = f a -> a

type Coalgebra f a = a -> f a


type RAlgebra f a = f (Fix f, a) -> a

type RCoalgebra f a = a -> f (Either (Fix f) a)


-- | <https://en.wikipedia.org/wiki/Catamorphism Catamorphism>. A generalization of folds.
cata :: Functor f => Algebra f a -> Fix f -> a
cata g = g . fmap (cata g) . out

-- | <https://en.wikipedia.org/wiki/Anamorphism Anamorphism>. A generalization of unfolds.
ana :: Functor f => Coalgebra f a -> a -> Fix f
ana c = In . fmap (ana c) . c


-- paramorphism
para :: Functor f => RAlgebra f a -> Fix f -> a
para r = r . fmap (id &&& para r) . out


bottomUp, topDown :: Functor f => (Fix f -> Fix f) -> Fix f -> Fix f
bottomUp g = g . In . fmap (bottomUp g) . out
topDown  g = In . fmap (topDown g) . out . g


class Functor f => Fixable f a | a -> f, f -> a where
  foldFix :: Algebra f a
  unfoldFix :: Coalgebra f a

  fromFix :: Fix f -> a
  fromFix = cata foldFix

  toFix :: a -> Fix f
  toFix = ana unfoldFix


catamap :: Fixable f a => Algebra f b -> a -> b
catamap alg = cata alg . toFix

anamap :: Fixable f a => Coalgebra f a -> a -> a
anamap coalg = fromFix . ana coalg


data (:+:) f g a = SumL (f a)
                 | SumR (g a)

deriving instance (Functor f, Functor g) => Functor (f :+: g)
deriving instance (Foldable f, Foldable g) => Foldable (f :+: g)
deriving instance (Traversable f, Traversable g) => Traversable (f :+: g)

deriving instance (Show (f a), Show (g a)) => Show ((f :+: g) a)
deriving instance (Eq (f a), Eq (g a)) => Eq ((f :+: g) a)


sumFixL :: Functor f => Fix f -> Fix (g :+: f)
sumFixL = In . SumR . fmap sumFixL . out

sumFixR :: Functor f => Fix f -> Fix (f :+: g)
sumFixR = In . SumL . fmap sumFixR . out
