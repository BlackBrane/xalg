module XAlg.Languages.Arithmetic.Expr (
-- * Core data types
  Expr (..),
  Expr_ (..),

-- * Parsing
  lit,
  term,
  expr,

-- * Evaluation
  eval

) where

import XAlg.Languages.Arithmetic.Number
import XAlg.Foundation.Recursion
import XAlg.Foundation.Core
import XAlg.Foundation.Parsing

import Data.Set
import Data.Data
import qualified Data.Map as M

import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Expr
import Text.Megaparsec.Lexer (float)
import qualified Data.Complex as C

-- | Arithmetical expression.
data Expr = Lit Number
          | Var String
          | Plus Expr Expr
          | Minus Expr Expr
          | Mult Expr Expr
          | Div Expr Expr
          | Power Expr Expr
          | Neg Expr
          deriving (Show, Eq, Ord, Data)

infixl 6 `Plus`
infixl 6 `Minus`
infixl 8 `Mult`
infixl 8 `Div`
infixl 9 `Power`


instance Num Expr where
  (*) = Mult
  (+) = Plus
  negate = Neg
  abs = undefined
  signum = undefined
  fromInteger = Lit . Int


instance RecursiveData Expr where
  nodeEquality (Lit x)     (Lit y)     = x == x
  nodeEquality (Var x)     (Var y)     = x == x
  nodeEquality (Plus _ _)  (Plus _ _)  = True
  nodeEquality (Minus _ _) (Minus _ _) = True
  nodeEquality (Mult _ _)  (Mult _ _)  = True
  nodeEquality (Div _ _)   (Div _ _)   = True
  nodeEquality (Power _ _) (Power _ _) = True
  nodeEquality (Neg _)     (Neg _)     = True
  nodeEquality _           _           = False

  children (Lit x)       = []
  children (Var x)       = []
  children (Plus x1 x2)  = [x1,x2]
  children (Minus x1 x2) = [x1,x2]
  children (Mult x1 x2)  = [x1,x2]
  children (Div x1 x2)   = [x1,x2]
  children (Power x1 x2) = [x1,x2]
  children (Neg x)       = [x]

  mapChildren f (Lit x)       = Lit x
  mapChildren f (Var x)       = Var x
  mapChildren f (Plus x1 x2)  = recursively f x1 `Plus` recursively f x2
  mapChildren f (Minus x1 x2) = recursively f x1 `Minus` recursively f x2
  mapChildren f (Mult x1 x2)  = recursively f x1 `Mult` recursively f x2
  mapChildren f (Div x1 x2)   = recursively f x1 `Div` recursively f x2
  mapChildren f (Power x1 x2) = recursively f x1 `Power` recursively f x2
  mapChildren f (Neg x)       = Neg (recursively f x)


instance SymbolicLang Expr Number where
  variable = Var
  isVariable (Var v) = Just v
  isVariable _       = Nothing
  literal = Lit
  isLiteral (Lit x) = Just x
  isLiteral _       = Nothing
  evaluate = eval


-----------------------------------------------------------------------------
--    Functorial data type
-----------------------------------------------------------------------------

data Expr_ a = Lit_ Number
             | Var_ String
             | Plus_ a a
             | Minus_ a a
             | Mult_ a a
             | Div_ a a
             | Power_ a a
             | Neg_ a
             deriving (Functor, Foldable, Traversable, Data)


instance Fixable Expr_ Expr where
  foldFix (Lit_ x) = Lit x
  foldFix (Var_ x) = Var x
  foldFix (Plus_ x1 x2) = Plus x1 x2
  foldFix (Minus_ x1 x2) = Minus x1 x2
  foldFix (Mult_ x1 x2) = Mult x1 x2
  foldFix (Div_ x1 x2) = Div x1 x2
  foldFix (Power_ x1 x2) = Power x1 x2
  foldFix (Neg_ x) = Neg x

  unfoldFix (Lit x) = Lit_ x
  unfoldFix (Var x) = Var_ x
  unfoldFix (Plus x1 x2) = Plus_ x1 x2
  unfoldFix (Minus x1 x2) = Minus_ x1 x2
  unfoldFix (Mult x1 x2) = Mult_ x1 x2
  unfoldFix (Div x1 x2) = Div_ x1 x2
  unfoldFix (Power x1 x2) = Power_ x1 x2
  unfoldFix (Neg x) = Neg_ x

instance ParaSymb Expr_ where
  isVariable_ (Var_ v) = Just v
  isVariable_ _        = Nothing
  variable_ = Var_



deriving instance Show a => Show (Expr_ a)
deriving instance Show (Fix Expr_)

deriving instance Eq a => Eq (Expr_ a)
deriving instance Eq (Fix Expr_)

-----------------------------------------------------------------------------
--    Parsing
-----------------------------------------------------------------------------

imaginaryFloat :: Parser (C.Complex Double)
imaginaryFloat = fmap (0 C.:+)
  $ char 'i' *> float <|> float <* char 'i'

imaginaryInt :: Parser (C.Complex Double)
imaginaryInt = fmap ((0 C.:+) . fromInteger)
  $ char 'i' *> integ <|> integ <* char 'i'

imaginary :: Parser (C.Complex Double)
imaginary = imaginaryInt <|> imaginaryFloat <?> "imaginary number"

-- | Parse a numeric literal.
lit :: Parser Number
lit = try (Complex <$> imaginary)
  <|> try (Real <$> float)
  <|> Int <$> integ

-- | Parser an arithmetic expression term.
term :: Parser Expr
term = parens expr <|> var <|> Lit <$> lit <?> "term"

-- | Parse an arithmetic expression.
expr :: Parser Expr
expr = makeExprParser term table <?> "expression"

-- Must be a better way to get spaces:
table :: [[Operator Parser Expr]]
table = [ prefix "-" Neg
        , binary "^" Power
        , binary "*" Mult ++ binary "/" Div
        , binary "+" Plus ++ binary "-" Minus
        ]


instance Parsable Expr where
  exprParser = expr



-----------------------------------------------------------------------------
--    Pretty printing
-----------------------------------------------------------------------------

-- Only Minus is special, second argument has higher precedence.
showNodes :: Algebra Expr_ (Int, String)
showNodes (Lit_ x)       = (5, show x)
showNodes (Var_ x)       = (5, x)
showNodes (Plus_ x1 x2)  = (1, applyParens 1 x1 ++ " + " ++ applyParens 1 x2)
showNodes (Mult_ x1 x2)  = (3, applyParens 3 x1 ++ "*" ++ applyParens 3 x2)
showNodes (Neg_ x)       = (5, "-" ++ applyParens 5 x)
showNodes (Minus_ x1 x2) = (1, applyParens 1 x1 ++ " - " ++ applyParens 2 x2)
showNodes (Div_ x1 x2)   = (3, applyParens 3 x1 ++ "/" ++ applyParens 3 x2)
showNodes (Power_ x1 x2) = (4, applyParens 4 x1 ++ "^" ++ applyParens 4 x2)

-- target prec, (node prec, shown term) -> apply parens if necessary
applyParens :: Int -> (Int,String) -> String
applyParens t (n,str) = if n < t then parenthesize str else str

parenthesize :: String -> String
parenthesize s = "(" ++ s ++ ")"


instance Renderable Expr where
  render = snd . catamap showNodes

instance Transcribe Expr

instance XAlgLang Expr Expr_ Number


-------


countNode :: Algebra Expr_ Int
countNode (Lit_ x)       = 1
countNode (Var_ x)       = 1
countNode (Plus_ x1 x2)  = x1 + x2
countNode (Mult_ x1 x2)  = x1 + x2
countNode (Minus_ x1 x2) = x1 + x2
countNode (Div_ x1 x2)   = x1 + x2
countNode (Power_ x1 x2) = x1 + x2
countNode (Neg_ x)       = succ x


countNodes :: Expr -> Int
countNodes = catamap countNode


-------

data Mode = IntMode
          | RealMode
          | ComplexMode deriving (Eq,Ord)

instance Show Mode where
  show IntMode     = "ℤ"
  show RealMode    = "ℝ"
  show ComplexMode = "ℂ"

litMode :: Number -> Mode
litMode (Real _)   = RealMode
litMode (Int   _)   = IntMode
litMode (Complex _) = ComplexMode

modeNodes :: Algebra Expr_ Mode
modeNodes (Lit_ x)       = litMode x
modeNodes (Var_ x)       = IntMode
modeNodes (Plus_ x1 x2)  = x1 `max` x2
modeNodes (Minus_ x1 x2) = x1 `max` x2
modeNodes (Mult_ x1 x2)  = x1 `max` x2
modeNodes (Div_ x1 x2)   = x1 `max` x2
modeNodes (Power_ x1 x2) = x1 `max` x2
modeNodes (Neg_ x)       = x


---------


evalNode :: Algebra Expr_ (EvalResult Expr Number)
evalNode (Lit_ x)       = ValueResult x
evalNode (Var_ x)       = FunctionResult (singleton x) (Var x)
evalNode (Plus_ x1 x2)  = combineWith Plus (+) x1 x2
evalNode (Minus_ x1 x2) = combineWith Minus (-) x1 x2
evalNode (Mult_ x1 x2)  = combineWith Mult (*) x1 x2
evalNode (Div_ x1 x2)   = combineWith Div (/) x1 x2
evalNode (Power_ x1 x2) = combineWith Power (**) x1 x2
evalNode (Neg_ x)       = applyWith Neg negate x

evalNodeCtx :: Env Expr -> Algebra Expr_ (EvalResult Expr Number)
evalNodeCtx m (Var_ s) = case M.lookup s m of
  Just x  -> catamap (evalNodeCtx m) x
  Nothing -> FunctionResult (singleton s) (Var s)
evalNodeCtx _ x = evalNode x

eval :: Expr -> EvalResult Expr Number
eval = catamap evalNode


-- helpers

type BinaryOp a = a -> a -> a
type UnaryOp a = a -> a

combineWith :: SymbolicLang x s => BinaryOp x -> BinaryOp s -> BinaryOp (EvalResult x s)
combineWith f g (FunctionResult vs x) (FunctionResult ws y) = FunctionResult (union vs ws) (f x y)
combineWith f g (FunctionResult vs x) (ValueResult t)       = FunctionResult vs $ f x $ literal t
combineWith f g (ValueResult s)       (FunctionResult ws y) = FunctionResult ws $ f (literal s) y
combineWith f g (ValueResult s)       (ValueResult t)       = ValueResult $ g s t

applyWith :: SymbolicLang x s => UnaryOp x -> UnaryOp s -> UnaryOp (EvalResult x s)
applyWith f g (FunctionResult s x) = FunctionResult s (f x)
applyWith f g (ValueResult r)      = ValueResult (g r)
