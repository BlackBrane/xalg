{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-} -- Needed to make `cabal haddock` work

module XAlg.Examples where

import XAlg.Foundation.Core
import XAlg.Infra.Rewrite
import XAlg.Languages.Arithmetic

x1 :: Lang Expr
x1 = [xalg| 1+2 |]

x2 :: Expr
x2 = [xexpr| x+2 |]

x3 :: Expr
x3 = [xexpr| (x^y)+2 |]

ex1 :: Expr
ex1 = [xexpr| x*y^3-4*y |]

ex2 :: Expr
ex2 = [xexpr| w^5+q^4 |]

pex :: Expr -> Maybe Expr
pex [xexpr| x-z |] = Just x
pex _              = Nothing

matchTest :: Maybe (Env Expr)
matchTest = match x2 x3

plusComm :: Equation Expr
plusComm = [xexpr| x+y |] :=: [xexpr| y+x |]


xp0 :: Expr
xp0 = [xexpr| x+0 |]



plusZeroRightId :: Equation Expr
plusZeroRightId = [xexpr| x+0 |] :=: [xexpr| x |]

x4 :: Expr
x4 = [xexpr| 2+2+0 |]

x5 :: Maybe Expr
x5 = rewrite plusZeroRightId x4

x6 :: Expr
x6 = [xexpr| w+0 |]

x6' :: Maybe Expr
x6' = rewrite plusZeroRightId x6
