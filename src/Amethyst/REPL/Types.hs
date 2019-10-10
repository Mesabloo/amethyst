{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}

module Amethyst.REPL.Types where

import System.Console.Haskeline
import Control.Monad.State
import Control.Monad.Except
import Amethyst.Interpreter.Types

type REPLError = String
type REPL = InputT (StateT EvalState (ExceptT REPLError IO))

instance MonadException m => MonadException (ExceptT e m) where
    controlIO f = ExceptT $ controlIO $ \(RunIO run) ->
        let run' = RunIO (fmap ExceptT . run . runExceptT)
        in  runExceptT <$> f run'

instance MonadException m => MonadException (StateT s m) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) ->
        let run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
        in  flip runStateT s <$> f run'

instance MonadError REPLError REPL where
    throwError = lift . lift . throwError
    m `catchError` h = do
        let s = runInputT defaultSettings m
        lift $ s `catchError` (runInputT defaultSettings . h)

instance MonadState EvalState REPL where
    get = lift get
    put = lift . put