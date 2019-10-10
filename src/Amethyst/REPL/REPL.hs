{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module Amethyst.REPL.REPL where

import Amethyst.REPL.Types
import Control.Monad.State
import Control.Monad.Except
import System.Console.Haskeline
import System.Exit
import Amethyst.Language.Parser
import qualified Text.Megaparsec as Mega
import Amethyst.Interpreter.Types
import Amethyst.Interpreter.Evaluator
import qualified Data.Text as Text
import Data.Functor
import Control.Lens
import Amethyst.REPL.Logger

runREPL :: REPL a -> IO (Either REPLError a)
runREPL r = runExceptT (evalStateT (runInputT defaultSettings r) initEvalState)

launchREPL :: REPL ()
launchREPL = withInterrupt (handleInterrupt launchREPL (forever run))

run :: REPL ()
run = do
    getInputLine "> " >>= \case
        Nothing -> liftIO (green (putStrLn "Goodbye!") *> exitSuccess)
        Just i -> catchError (exec i) handleError
    s <- use stack
    liftIO $ blue (print s)
  where
    handleError e = liftIO $ (red . bold) (putStrLn e)

exec :: String -> REPL ()
exec input = do
    exprs <- liftEitherMap (throwError . Mega.errorBundlePretty) (Mega.runParser program "interactive" (Text.pack input))
    evalState' <- get
    (s, res) <- liftIO (runEval (eval exprs) evalState')
    case res of
        Left e -> throwError e
        Right x -> put s

liftEitherMap :: MonadError e m => (f -> m a) -> Either f a -> m a
liftEitherMap f (Left e) = f e
liftEitherMap f (Right x) = pure x