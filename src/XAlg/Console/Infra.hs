module XAlg.Console.Infra where

import XAlg.Console.Environment
import XAlg.Languages.Arithmetic

import System.Console.Haskeline
import Control.Monad.Trans (liftIO)
import System.IO
import System.Console.Ansigraph.Core
import System.Console.ANSI
import Control.Monad.State
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))


type XAlgM = StateT (XState Expr) (InputT IO)


---- lifted helper ops ----

printStrLn :: MonadIO m => String -> m ()
printStrLn = liftIO . putStrLn

printStr :: MonadIO m => String -> m ()
printStr = liftIO . putStr

line :: MonadIO m => m ()
line = printStrLn ""

print' :: (MonadIO m, Show a) => a -> m ()
print' = liftIO . print

nobuff :: MonadIO m => m ()
nobuff = liftIO $ hSetBuffering stdin NoBuffering

clearLine' :: MonadIO m => m ()
clearLine' = liftIO $ putChar '\r' *> clearLine

eitherMayb :: Either a b -> Maybe b
eitherMayb = either (const Nothing) Just

---- replmap Infrastructure ----

quit :: XAlgM ()
quit = printStrLn "Farewell.. \n" *> return ()

invalid :: XAlgM ()
invalid = do
  printStrLn "*****  Unrecognized command."
  printStrLn "*****  Enter ':?' for help."


---- Command infra ----

data CommandMap = CommandMap { runCmdMap :: String -> Maybe (XAlgM ()) }

instance Monoid CommandMap where
  mempty = CommandMap (const Nothing)
  mappend x y = CommandMap $ \s -> runCmdMap x s <|> runCmdMap y s



command :: XAlgM () -> CommandMap -> String -> XAlgM ()
command m c s = fromMaybe m (runCmdMap c s)

promptCmd :: String -> XAlgM () -> CommandMap -> XAlgM ()
promptCmd p m c = prompt p >>= command m c



infixr 3 `contWith`  -- compare to: infixr 6 <>
infixl 3 `withCont`

-- | Modify a CommandMap by appending a new action, run only when the CM fires.
contWith :: CommandMap -> XAlgM () -> CommandMap
contWith (CommandMap f) m = CommandMap $ fmap (*> m) . f

-- | Flipped argument version of `contWith`
withCont :: XAlgM () -> CommandMap -> CommandMap
withCont = flip contWith


quitCmd :: CommandMap
quitCmd = CommandMap $ \str -> case str of
  ":q"    -> Just quit
  ":quit" -> Just quit
  _       -> Nothing

exitTo :: XAlgM () -> CommandMap
exitTo m = CommandMap $ \str -> case str of
  ":e"    -> Just m
  ":exit" -> Just m
  _       -> Nothing



---- ANSI/haskeline helpers ----

promptColor :: Coloring
promptColor = Coloring (pure $ AnsiColor Vivid Cyan) Nothing

exploreColor :: Coloring
exploreColor = Coloring (pure $ AnsiColor Dull Green) Nothing

boldPrompt :: MonadException m => Coloring -> String -> InputT m (Maybe String)
boldPrompt c s = boldStr c s *> getInputLine ""

prompt' :: MonadException m => String -> InputT m String
prompt' p = do
  mstr <- boldPrompt promptColor p
  case mstr of
    Just s  -> return s
    Nothing -> error "Input error."

prompt :: String -> XAlgM String
prompt = lift . prompt'

rawClr :: Coloring
rawClr = Coloring (Just (AnsiColor Dull Yellow)) (Just (AnsiColor Vivid Black))

banner :: String
banner = unlines
  [ "           __"
  , "     __   / /   ____   ___"
  , "     \\ \\ / /   /   || / //__ _"
  , "      \\   /   / /| ||/ / __ `//"
  , "      /   \\  / ___ |/ / /_/ //"
  , "     / / \\ \\/_// |_/_/\\__, //"
  , "    /_/   \\_\\        /____//" ]

-- Easily convert banner to escaped format with 'readFile "banner.txt" >>= print . lines'
