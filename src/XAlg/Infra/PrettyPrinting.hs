{-# LANGUAGE OverloadedStrings #-}

module XAlg.Infra.PrettyPrinting where

import XAlg.Foundation.Recursion
import XAlg.Foundation.Core
import XAlg.Foundation.Focusing
import XAlg.Languages.Arithmetic

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>),black)
import qualified Text.PrettyPrint.ANSI.Leijen  as L ((<$>),black)
import Control.Monad.IO.Class (MonadIO, liftIO)


infixr 5 <->

(<->) :: Doc -> Doc -> Doc
(<->) = (L.<$>)


pprint :: MonadIO m => Pretty a => a -> m ()
pprint = liftIO . print . pretty

putDocLn :: MonadIO m => Doc -> m ()
putDocLn d = liftIO . putDoc $ d <> hardline

pad :: Doc -> Doc
pad = enclose space space

pshow = text . show

black = dullblack
gray = black

litColor = green  -- use dull versions for de-focused
varColor = magenta
fgColor = white
emph = bold . underline

prettyNodes :: Algebra Expr_ (Int,Doc)
prettyNodes (Lit_ x)       = (5, litColor . pshow $ x)
prettyNodes (Var_ x)       = (5, varColor . text $ x)
prettyNodes (Plus_ x1 x2)  = (1, applyPar 1 x1 <> fgColor " + " <> applyPar 1 x2)
prettyNodes (Minus_ x1 x2) = (1, applyPar 1 x1 <> fgColor "-" <> applyPar 2 x2)
prettyNodes (Mult_ x1 x2)  = (3, applyPar 3 x1 <> fgColor "*" <> applyPar 3 x2)
prettyNodes (Div_ x1 x2)   = (3, applyPar 3 x1 <> fgColor "/" <> applyPar 3 x2)
prettyNodes (Power_ x1 x2) = (4, applyPar 4 x1 <> fgColor "^" <> applyPar 4 x2)
prettyNodes (Neg_ x)       = (5, fgColor "-" <> applyPar 5 x)


instance Pretty Expr where
  pretty = ondullblack
         . enclose (linebreak <> linebreak) linebreak
         . enclose space space
         . snd
         . catamap prettyNodes

-- target prec, (node prec, shown term) -> apply parens if necessary
applyPar :: Int -> (Int,Doc) -> Doc
applyPar t (n,d) = if n < t then parenthesize' d else d

parenthesize' :: Doc -> Doc
parenthesize' = enclose (fgColor "(") (fgColor ")")

prettyFocusNodes :: Algebra (Doubled Expr_) (Int,Doc)
prettyFocusNodes (SumL x) = prettyNodes x
prettyFocusNodes (SumR x) = onblack . emph <$> prettyNodes x

instance Pretty (Focused Expr_) where
  pretty = ondullblack
--         . enclose (linebreak <> linebreak) linebreak
         . enclose space space
         . snd
         . cata prettyFocusNodes

newtype Padded a = Pad a

instance Pretty a => Pretty (Padded a) where
  pretty = enclose (linebreak <> linebreak) linebreak . pretty

padPPrint :: MonadIO m => Pretty a => a -> m ()
padPPrint = liftIO . print . pretty . Pad
