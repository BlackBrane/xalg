module XAlg.Infra.Analyze (
  context,
  argCount
) where

import XAlg.Foundation.Recursion
import XAlg.Foundation.Core

import Data.Set  (Set, union, fromList, empty)



ctx :: XAlgLang x f s => Algebra f (Set String)
ctx (Variable_ x) = fromList [x]
ctx x = foldr union empty (children_ x)

context :: XAlgLang x f s => x -> Set String
context = catamap ctx

argCount :: XAlgLang x f s => x -> Int
argCount = length . context


-----


{-
ctx :: Algebra Expr_ (Set String)
ctx (Lit_ x)       = empty
ctx (Var_ x)       = fromList [x]
ctx (Neg_ x)       = x
ctx (Plus_ x1 x2)  = x1 `union` x2
ctx (Mult_ x1 x2)  = x1 `union` x2
ctx (Minus_ x1 x2) = x1 `union` x2
ctx (Div_ x1 x2)   = x1 `union` x2
ctx (Power_ x1 x2) = x1 `union` x2

context :: Expr -> Set String
context = catamap ctx
-}
