module XAlg.Console.Environment where

import XAlg.Foundation.Core
import XAlg.Foundation.Recursion
import XAlg.Languages.Arithmetic
import XAlg.Infra.Logic

import qualified Data.Map as Map
import Data.Maybe (fromMaybe, catMaybes)


-- all this to be generalized

data XState a = XState
  { env :: Env (Lang a)
  , that :: Maybe a }


istate :: XState Expr
istate = XState startEnv Nothing

startEnv :: Env (Lang Expr)
startEnv = Map.fromList $ ("xpr1", EXPR xpr1) : (fmap EQUATION . toPair <$> ringLaws)

xpr1 :: Expr
xpr1 = Mult (Mult (Lit 2) (Var "x")) (Power (Var "e") (Minus (Power (Var "x") (Lit 2)) (Var "a")))



fullEnv :: XState a -> Env (Lang a)
fullEnv st = case that st of
  Just x  -> Map.insert "that" (EXPR x) (env st)
  Nothing -> env st

exprEnv :: XState a -> Env a
exprEnv = Map.mapMaybe exprPrism . fullEnv

eqnEnv :: XState a -> Env (Equation a)
eqnEnv = Map.mapMaybe eqnPrism . fullEnv


bind :: Binding a -> XState a -> XState a
bind (Binding str x) state = state { env = Map.insert str (EXPR x) (env state) }

setThat :: a -> XState a -> XState a
setThat x state = state { that = Just x }


-----


-- | Given a map from strings to expressions, the algebra that plugs in any variables
--   present in the expression with the values associated by the map, when present.
expand :: XAlgLang x f s => XState x -> Algebra f x
expand st v@(Variable_ name) = fromMaybe (variable name) $ Map.lookup name (exprEnv st)
expand _  xpr                = foldFix xpr
