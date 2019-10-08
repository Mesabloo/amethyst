{-# LANGUAGE LambdaCase, TypeApplications, OverloadedStrings #-}

module Amethyst.Interpreter.Stdlib where

import qualified Data.Map as Map
import qualified Data.Text as Text
import Amethyst.Interpreter.Types
import Amethyst.Language.Parser
import Polysemy.Error
import Polysemy.State
import Control.Monad

defaultEnv :: Map.Map Text.Text Value
defaultEnv = Map.fromList
    [ ("+", VNative addE)
    , ("=", VNative assE)
    , ("-", VNative subE)
    , ("swap", VNative swapE)
    , ("dup", VNative dupE)
    , ("pop", VNative popE)
    , (">=", VNative goeE) ]

pop :: Sem' Value
pop = gets _stack >>= \case
    x:xs -> (x <$) . modify $ \st -> st { _stack = xs }
    []   -> throw @EvalError "Cannot pop an empty stack."

push :: Value -> [Value] -> [Value]
push = (:)
{-# INLINE push #-}

addE :: Sem' Value
addE = do
    i1 <- extract @Integer =<< pop
    i2 <- extract @Integer =<< pop
    let val = VAtom (EInt (i2 + i1))
    (val <$) . modify $ \st -> st { _stack = push val (_stack st) }

assE :: Sem' Value
assE = do
    e1 <- pop
    Id i <- extract @Id =<< pop
    (e1 <$) . modify $ \st -> st { _env = Map.insert i e1 (_env st) }

subE :: Sem' Value
subE = do
    i1 <- extract @Integer =<< pop
    i2 <- extract @Integer =<< pop
    let val = VAtom (EInt (i2 - i1))
    (val <$) . modify $ \st -> st { _stack = push val (_stack st) }

swapE :: Sem' Value
swapE = do
    t1 <- pop
    t2 <- pop
    (t2 <$) . modify $ \st -> st { _stack = push t2 (push t1 (_stack st)) }

dupE :: Sem' Value
dupE = do
    t <- pop
    (t <$) . modify $ \st -> st { _stack = push t (push t (_stack st)) }

popE :: Sem' Value
popE = pop

goeE :: Sem' Value
goeE = do
    t1 <- extract @Integer =<< pop
    t2 <- extract @Integer =<< pop
    let val = VAtom (EInt (if t2 >= t1 then 1 else 0))
    (val <$) . modify $ \st -> st { _stack = push val (_stack st) }