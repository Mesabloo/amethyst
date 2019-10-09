{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE DataKinds, LambdaCase, FlexibleContexts, TypeApplications, OverloadedStrings, GADTs #-}

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
import Control.Lens
import Data.Maybe

initEvalState :: EvalState
initEvalState = EvalState
    { _stack = mempty
    , _env = defaultEnv
                <> Map.singleton "?:" (VNative ifE)
    }

---------------------------------------------------------------------

eval :: Block -> Eval ()
eval [] = pure ()
eval (x:xs) = exec x *> eval xs
  where
    exec (EAtom a) = stack %= (VAtom a  :)
    exec (EId (Just _) name) = stack %= (VId (Id name) :)
    exec (EId Nothing name) =
        let err = throw @EvalError ("Function `" <> Text.unpack name <> "` not found.")
        in env `uses` Map.lookup name >>= maybe err evalVal
    exec (EBlock es) = stack %= (VBlock es :)

evalVal :: Value -> Eval ()
evalVal (VNative f) = f
evalVal (VBlock vs) = isolated (eval vs)
evalVal v = stack %= (v :)

isolated :: Eval a -> Eval a
isolated action = do
    e <- use env
    action <* (env .= e)

top :: [Value] -> Eval Value
top [] = throw @EvalError "Empty stack when using `top`."
top (x:xs) = pure x

----------------------------------------------------------------------

runEval :: Eval a -> EvalState -> IO (EvalState, Either EvalError a)
runEval sem s = sem & runError & runState s & runM

----------------------------------------------------------------------

ifE :: Eval ()
ifE = do
    cond' <- extract' @Integer =<< pop
    _then <- pop
    _else <- pop
    eval =<< extract' @Block (if cond' /= 0 then _then else _else)