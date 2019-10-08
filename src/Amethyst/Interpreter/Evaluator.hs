{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE DataKinds, LambdaCase, FlexibleContexts, TypeApplications, OverloadedStrings #-}

module Amethyst.Interpreter.Evaluator where

import Polysemy.State
import Polysemy.IO
import Polysemy.Error
import Polysemy
import Amethyst.Language.Parser
import qualified Data.Map as Map
import qualified Data.Text as Text
import Control.Monad
import Data.Function ((&))
import Amethyst.Interpreter.Stdlib
import Amethyst.Interpreter.Types
import Debug.Trace

initEvalState :: EvalState
initEvalState = EvalState
    { _stack = mempty
    , _env = defaultEnv
                <> Map.singleton "?:" (VNative ifE)
    }

---------------------------------------------------------------------

eval :: Eval Value
eval [] = join (gets (top . _stack))
eval (x:xs) =
    let exec = \case
            EAtom a ->
                let val = VAtom a
                in (val <$) . modify $ \st -> st { _stack = push val (_stack st) }
            EId (Just _) name ->
                let val = VId name
                in (val <$) . modify $ \st -> st { _stack = push val (_stack st) }
            EId Nothing name ->
                gets (Map.lookup name . _env) >>= \case
                    Just x -> evalVal x
                    Nothing -> throw @EvalError ("Function `" <> Text.unpack name <> "` not found.")
            EBlock es ->
                let b = VBlock es
                in (b <$) . modify $ \st -> st { _stack = push b (_stack st) }
    in foldl ((. exec) . (*>)) (exec x) xs

evalVal :: Value -> Sem' Value
evalVal (VNative f) = f
evalVal (VBlock vs) = eval vs
evalVal v = (v <$) . modify $ \st -> st { _stack = push v (_stack st) }

top :: Member (Error String) r => [Value] -> Sem r Value
top [] = throw @EvalError "Empty stack when using `top`."
top (x:xs) = pure x

push :: Value -> [Value] -> [Value]
push v s = (v : s)

----------------------------------------------------------------------

runEval :: Sem' Value -> IO (EvalState, Either EvalError Value)
runEval sem = sem & runError & runState initEvalState & runM

----------------------------------------------------------------------

ifE :: Sem' Value
ifE = gets _stack >>= \case
    cond:_then:_else:xs -> do
        cond' <- extractInt cond
        modify $ \st -> st { _stack = xs }
        eval =<< if cond' /= 0 then extractBlock _then else extractBlock _else