module XAlg.Console.REPL
--  (main,repl)
  where

import XAlg.Foundation.Core
import XAlg.Foundation.Recursion
import XAlg.Foundation.Parsing
import XAlg.Foundation.Focusing
import XAlg.Languages.Arithmetic
import XAlg.Infra.Analyze
import XAlg.Infra.PrettyPrinting
import XAlg.Infra.Logic
import XAlg.Infra.Rewrite (rewrite')
import XAlg.Console.Environment
import XAlg.Console.Infra

import Text.Megaparsec
import System.Console.Haskeline
import Control.Monad.Trans (liftIO)
import System.Console.Ansigraph.Core
import System.Console.ANSI

import Control.Monad.State
import Control.Monad (void)
import Data.Monoid ((<>))

import Data.Set
import qualified Data.Map as Map


runXAlg :: XAlgM a -> IO ()
runXAlg m = void . runInputT defaultSettings . runStateT m $ istate

---- Main ----

main :: IO ()
main = do
  putStrLn banner
  runXAlg repl


getEnv :: XAlgM (Env (Lang Expr))
getEnv = env <$> get

---- Command line progs ----

replCmds :: CommandMap
replCmds = CommandMap $ \s -> case s of
  (words -> [":explore"])  -> Just explore'
  (words -> ":explore":ws) -> explore . initFocus <$> (eitherMayb . parse (full expr) "" . unwords) ws
  ":env"                   -> Just $ printStrLn "Environment contents: " *> reportEnv *> line *> repl
  _                        -> Nothing

evalCmd :: CommandMap
evalCmd = CommandMap (pure . updater) `contWith` repl

-- parse the supplied string, save any bindings in the xstate, display the expanded expression
updater :: String -> XAlgM ()
updater str = case parse lang "" str of
  Left err -> liftIO . putStrLn . processErr . parseErrorPretty $ err
  Right l  -> update l

update :: Lang Expr -> XAlgM ()
update (EQUATION e) = printStrLn (show e)
update (EXPR x)     = do
  st <- get
  let y = catamap (expand st) x
  report y *> modify (setThat y)

bindCmd :: CommandMap
bindCmd = withCont repl . CommandMap $ \str -> case parse binding "" str of
  Left err -> Nothing
  Right b  -> Just $ modify $ bind b


reportEnv :: XAlgM ()
reportEnv = mapM_ printStrLn =<< fmap (render . fromPair) . Map.toList <$> getEnv



replCmd :: CommandMap
replCmd = quitCmd <> replCmds <> bindCmd <> evalCmd


repl :: XAlgM ()
repl = promptCmd "XAlg> " (invalid *> repl) replCmd



-- Display a term to the screeen after plugging in context
report :: Expr -> XAlgM ()
report x = do
    pprint x
    line
    printRaw x
    line
    s <- get
    let cx = context x
    let keys = Map.keysSet (env s)
    let bcx = keys `intersection` cx
    let ucx = cx `difference` bcx
    case toList ucx of
         l@(x:xs) -> printStrLn $ "Unbound context: " ++ show l
         _        -> printStrLn $ "Evaluates to: " ++ show (eval x)

printRaw :: Expr -> XAlgM ()
printRaw x = do
    printStr "Raw term: "
    colorStrLn rawClr $ show x




-------- Explore term --------


deriving instance Show (Focused Expr_)
deriving instance Eq (Focused Expr_)


data Direction = U | D | L | R

instance Show Direction where
  show U = "↑"
  show D = "↓"
  show L = "←"
  show R = "→"

move :: Traversable t => Direction -> Focused t -> Focused t
move U = moveUp
move D = moveDown
move L = moveLeft
move R = moveRight



detectArrow' :: IO (Either String Direction)
detectArrow' = do
  a <- getChar
  case a of
    '\ESC' -> do
      b <- getChar
      case b of
        '[' -> do
          c <- getChar
          case c of
            'A' -> return (Right U)
            'B' -> return (Right D)
            'C' -> return (Right R)
            'D' -> return (Right L)
            _   -> return (Left [a,b,c])
        _ -> return (Left [a,b])
    _ -> return (Left [a])

detectArrow :: IO (Either String Direction)
detectArrow = do
  mar <- detectArrow'
  clearLine'
  return mar


-- Display a term to the screeen after plugging in context
exploreView :: Focused Expr_ -> XAlgM ()
exploreView fx = do
    line
    pprint fx
    line
    printRawF fx
    line

printRawF :: Focused Expr_ -> XAlgM ()
printRawF fx = do
    printStrLn "Raw term (Vanilla): "
    colorStrLn rawClr $ show (collapseFocus fx)
    line
    printStrLn "Raw term (Functorial fixed-point): "
    colorStrLn rawClr $ show fx


explore' :: XAlgM ()
explore' = do
  th <- that <$> get
  case th of
       Just x  -> explore $ initFocus x
       Nothing -> printStrLn "Must enter an expression first."

explore :: Focused Expr_ -> XAlgM ()
explore fx = do
  nobuff
  line
  pprint fx
  line
  str_or_dir <- exploreInput ""
  case str_or_dir of
    Right d -> clearBack 3 *> explore (move d fx)
    Left str -> command (invalid *> explore fx) (exploreCmdMap fx) str

exploreCmdMap :: Focused Expr_ -> CommandMap
exploreCmdMap fx = quitCmd <> exitTo repl <> withCont (explore fx) (exploreCmds fx)

exploreCmds :: Focused Expr_ -> CommandMap
exploreCmds fx = CommandMap $ \s -> case s of
  (words -> [":rewrite",w]) -> Just $ do
    eqs <- eqnEnv <$> get
    let meq = Map.lookup w eqs
    case meq of
      Nothing -> do
        printStrLn $ "Equation '" ++ w ++ "' not found."
        explore fx
      Just eq -> printStrLn "FILL ME IN"
        -- explore $ toFix $ (transformFocus (rewrite' eq) fx)
  _                       -> Nothing


safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _      = Nothing

safeLast :: [a] -> Maybe a
safeLast = safeHead . reverse

safeInit :: [a] -> [a]
safeInit l@(x:xs) = init l
safeInit _        = []


exploreInput :: String -> XAlgM (Either String Direction)
exploreInput s = do
  boldStr exploreColor "XAlg-TermExplorer> "
  printStr s
  str_or_dir <- liftIO detectArrow
  clearLine'
  boldStr exploreColor "XAlg-TermExplorer> "
  printStr $ s ++ either id (const "") str_or_dir
  case str_or_dir of
    Right d -> return (Right d)
    Left str  -> case safeLast str of
      Just '\DEL' -> clearLine' *> exploreInput (safeInit s)
      Just '\n'   -> clearBack 1 *> return (Left s)
      Just _      -> clearLine' *> exploreInput (s ++ str)
      Nothing     -> exploreInput s
