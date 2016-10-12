-- | General parsing infrastructure to be used in constructing language-specific parsers.
module XAlg.Foundation.Parsing (
-- * Abstract language parser
  lang,
  binding,

-- * Generalities
  var,
  integ,

-- * Parser combinators
  parens,
  full,

-- * Operator parser building blocks
  prefix,
  binary,

-- * Error-reporting helpers
  processErr,
  reportErr

) where

import XAlg.Foundation.Core
import XAlg.Foundation.Focusing
import XAlg.Foundation.Recursion

import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Expr

import Control.Monad.IO.Class (MonadIO, liftIO)


---- General stuff ----

varName :: Parser String
varName = some lowerChar <?> "variable name"
-- change to allow x_999 format

-- | Parse a variable in an unspecified language.
var :: SymbolicLang x s => Parser x
var = variable <$> varName

-- | Parse an integer.
integ :: Parser Integer
integ = do
  ds <- some digitChar
  let n = read ds :: Integer
  return n

-- | Run any parser while requiring surrounding parentheses.
parens :: Parser a -> Parser a
parens psr = do
  char '('
  x <- psr
  char ')'
  return x

whitespace :: Parser String
whitespace = many (char ' ')

-- | Run any parser while requireing an end of input at the end.
full :: Parser a -> Parser a
full psr = do
  whitespace
  x <- psr
  whitespace
  eof
  return x


---- Parse language stuff ----



bindWith :: Parser x -> Parser (Binding x)
bindWith p = do
  string "let "
  name <- varName
  string " = "
  y <- p
  return $ Binding name y

binding :: Parsable x => Parser (Binding x)
binding = bindWith exprParser

eqWith :: Parser x -> Parser (Equation x)
eqWith p = do
  x1 <- p
  string " = "
  x2 <- p
  return $ x1 :=: x2

equation :: Parsable x => Parser (Equation x)
equation = eqWith exprParser

-- | Parse statement of an unspecified language: expressions, bindings and equations.
lang :: Parsable x => Parser (Lang x)
lang = full $  try (EQUATION <$> equation)
           <|> EXPR <$> exprParser


---- Expression Parser helpers ----

pad :: String -> String
pad str = " " ++ str ++ " "

-- | Create an operator parser, allowing for the op to either be padded by spaces or not.
binary :: String -> (x -> x -> x) -> [Operator Parser x]
binary name f = [ InfixL (f <$ string name)
                , InfixL (f <$ string (pad name)) ]

-- | Create a prefix operator parser.
prefix :: String -> (x -> x) -> [Operator Parser x]
prefix name f = [ Prefix (f <$ string name) ]




---- Error printing ----
-- | Convert a parser error string into a format chosen for readability.
processErr :: String -> String
processErr = unlines . ("Parse error: " :) . map ("   " ++) . lines

{-

reportErr :: (Ord t, ShowToken t, ShowErrorComponent e, MonadIO m) =>
             ParseError (Token s) e -> m ()
reportErr = liftIO . putStrLn . processErr . parseErrorPretty
-}



reportErr :: MonadIO m => String -> m ()
reportErr = liftIO . putStrLn . processErr
