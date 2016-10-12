{-# LANGUAGE QuasiQuotes #-}

module XAlg.Languages.Arithmetic.Laws where

import XAlg.Foundation.Recursion
import XAlg.Foundation.Core
import XAlg.Infra.Logic

import XAlg.Languages.Arithmetic.Expr
import XAlg.Languages.Arithmetic.Number
import XAlg.Languages.Arithmetic.Quotation


ringAxioms :: Axioms Expr
ringAxioms = Axioms . map (fmap Eqn) $ ringLaws


ringLaws :: [Named (Equation Expr)]
ringLaws =
  [
    -- Semigroup (+)
    Named "PlusAssociative" $
      [xexpr| (x+y)+z |] :=: [xexpr| x+(y+z) |]

    -- Monoid (+)
  , Named "PlusZeroRightId" $
      [xexpr| x+0 |] :=: [xexpr| x |]

  , Named "PlusZeroLeftId" $
      [xexpr| 0+x |] :=: [xexpr| x |]

    -- Group (+)
  , Named "PlusInverseRightZero" $
      [xexpr| x+(-x) |] :=: [xexpr| 0 |]

  , Named "PlusInverseLeftZero" $
      [xexpr| (-x)+x |] :=: [xexpr| 0 |]

    -- Abelian group (+)
  , Named "PlusCommutative" $
      [xexpr| x+y |] :=: [xexpr| y+x |]

    -- Semigroup (*)
  , Named "MultAssociative" $
      [xexpr| (x*y)*z |] :=: [xexpr| x*(y*z) |]

    -- Monoid (*)
  , Named "MultOneRightId" $
      [xexpr| x*0 |] :=: [xexpr| x |]

  , Named "MultOneLeftId" $
      [xexpr| 0*x |] :=: [xexpr| x |]

    -- Group (*)
  , Named "MultInverseRightOne" $
      [xexpr| x*(1/x) |] :=: [xexpr| 1 |]

  , Named "MultInverseLeftOne" $
      [xexpr| (1/x)*x |] :=: [xexpr| 1 |]

    -- Abelian group (*)
  , Named "MultCommutative" $
      [xexpr| x*y |] :=: [xexpr| y*x |]

  ]
