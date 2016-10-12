module XAlg.Languages.Arithmetic.Quotation where


import XAlg.Foundation.Core
import XAlg.Foundation.Parsing
import XAlg.Infra.Analyze

import XAlg.Languages.Arithmetic.Number
import XAlg.Languages.Arithmetic.Expr


import Language.Haskell.TH        (Q, Exp, Pat, mkName, varP, PatQ)
import Language.Haskell.TH.Quote  (QuasiQuoter (..), dataToPatQ)
import Language.Haskell.TH.Syntax (liftData)
import Text.Megaparsec            (parse, parseErrorPretty)
import Data.Generics              (extQ)
import Data.Set

---- XAlg Quoter ----

xalgQuoter :: String -> Q Exp
xalgQuoter str = case parse lang "" str of
  Left err -> fail $ parseErrorPretty err
  Right x  -> liftData (x :: Lang Expr)  -- generalize later

xalg :: QuasiQuoter
xalg = QuasiQuoter xalgQuoter undefined undefined undefined

---- Expr Quoter ----

xexpr :: QuasiQuoter
xexpr = QuasiQuoter xalgExprQuoter xalgExprPatQuoter undefined undefined

xalgExprQuoter :: String -> Q Exp
xalgExprQuoter str = case parse (full expr) "" str of
  Left err -> fail $ parseErrorPretty err
  Right x  -> liftData x

-- Expr Pattern Quoter --

xalgExprPatQuoter :: String -> Q Pat
xalgExprPatQuoter str = case parse (full expr) "" str of
  Left err -> fail $ parseErrorPretty err
  Right x  -> dataToPatQ (const Nothing `extQ` metaPat (context x)) x

metaPat :: Set String -> Expr -> Maybe PatQ
metaPat cx (Var x) | x `member` cx = Just (varP (mkName x))
metaPat _  _                       = Nothing



-- dataToPatQ :: Data a => (forall b. Data b => b -> Maybe (Q Pat)) -> a -> Q Pat
-- https://hackage.haskell.org/package/template-haskell-2.11.0.0/docs/src/Language.Haskell.TH.Syntax.html#dataToPatQ

-- extQ :: (Typeable a, Typeable b) => (a -> q) -> (b -> q) -> a -> q

{- data QuasiQuoter = QuasiQuoter
  { quoteExp :: String -> Q Exp,
    quotePat :: String -> Q Pat,
    quoteType :: String -> Q Type,
    quoteDec :: String -> Q [Dec] }   -}

-- https://www.well-typed.com/blog/2014/10/quasi-quoting-dsls/
-- https://www.schoolofhaskell.com/user/marcin/quasiquotation-101
