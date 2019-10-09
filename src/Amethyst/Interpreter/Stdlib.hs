{-# LANGUAGE LambdaCase, TypeApplications, OverloadedStrings #-}

module Amethyst.Interpreter.Stdlib where

import qualified Data.Map as Map
import qualified Data.Text as Text
import Amethyst.Interpreter.Types
import Amethyst.Language.Parser
import Polysemy.Error
import Polysemy.State
import Control.Monad
import Control.Lens

defaultEnv :: Map.Map Text.Text Value
defaultEnv = Map.fromList
    [ ("+", VNative addE)
    , ("=", VNative assE)
    , ("-", VNative subE)
    , ("swap", VNative swapE)
    , ("dup", VNative dupE)
    , ("pop", VNative popE)
    , (">=", VNative goeE)
    , ("*", VNative mulE)
    , ("<=", VNative loeE) ]

pop :: Sem' Value
pop = use stack >>= \case
    x:xs -> x <$ (stack .= xs)
    []   -> throw @EvalError "Cannot pop an empty stack."

push :: Value -> [Value] -> [Value]
push = (:)
{-# INLINE push #-}

addE :: Sem' Value
addE = do
    i1 <- extract' @Integer =<< pop
    i2 <- extract' @Integer =<< pop
    let val = VAtom (EInt (i2 + i1))
    val <$ (stack %= push val)

assE :: Sem' Value
assE = do
    e1 <- pop
    Id i <- extract' @Id =<< pop
    e1 <$ (env %= Map.insert i e1)

subE :: Sem' Value
subE = do
    i1 <- extract' @Integer =<< pop
    i2 <- extract' @Integer =<< pop
    let val = VAtom (EInt (i2 - i1))
    val <$ (stack %= push val)

swapE :: Sem' Value
swapE = do
    t1 <- pop
    t2 <- pop
    t2 <$ (stack %= (push t2 . push t1))

dupE :: Sem' Value
dupE = do
    t <- pop
    t <$ (stack %= (push t . push t))

popE :: Sem' Value
popE = pop

goeE :: Sem' Value
goeE = do
    t1 <- extract' @Integer =<< pop
    t2 <- extract' @Integer =<< pop
    let val = VAtom (EInt (if t2 >= t1 then 1 else 0))
    val <$ (stack %= push val)

mulE :: Sem' Value
mulE = do
    t1 <- extract' @Integer =<< pop
    t2 <- extract' @Integer =<< pop
    let val = VAtom (EInt (t2 * t1))
    val <$ (stack %= push val)

loeE :: Sem' Value
loeE = do
    t1 <- extract' @Integer =<< pop
    t2 <- extract' @Integer =<< pop
    let val = VAtom (EInt (if t2 <= t1 then 1 else 0))
    val <$ (stack %= push val)
