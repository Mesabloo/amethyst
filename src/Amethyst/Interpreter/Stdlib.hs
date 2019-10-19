{-# LANGUAGE LambdaCase, TypeApplications, OverloadedStrings #-}

module Amethyst.Interpreter.Stdlib where

import qualified Data.Map as Map
import qualified Data.Text as Text
import Amethyst.Interpreter.Types
import Amethyst.Language.Parser
import Polysemy.Error
import Polysemy.State
import Polysemy.Embed
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
    , ("<=", VNative loeE)
    , ("print", VNative printE) ]

pop :: Eval Value
pop = use stack >>= \case
    x:xs -> x <$ (stack .= xs)
    []   -> throw @EvalError "Cannot pop an empty stack."

addE :: Eval ()
addE = do
    i1 <- extract' @Integer =<< pop
    i2 <- extract' @Integer =<< pop
    let val = VAtom (EInt (i2 + i1))
    stack %= (val :)

assE :: Eval ()
assE = do
    e1 <- pop
    Id i <- extract' @Id =<< pop
    env %= Map.insert i e1

subE :: Eval ()
subE = do
    i1 <- extract' @Integer =<< pop
    i2 <- extract' @Integer =<< pop
    let val = VAtom (EInt (i2 - i1))
    stack %= (val :)

swapE :: Eval ()
swapE = do
    t1 <- pop
    t2 <- pop
    stack %= ([t2, t1] <>)

dupE :: Eval ()
dupE = do
    t <- pop
    stack %= ([t, t] <>)

popE :: Eval ()
popE = () <$ pop

goeE :: Eval ()
goeE = do
    t1 <- extract' @Integer =<< pop
    t2 <- extract' @Integer =<< pop
    let val = VAtom (EInt (if t2 >= t1 then 1 else 0))
    stack %= (val :)

mulE :: Eval ()
mulE = do
    t1 <- extract' @Integer =<< pop
    t2 <- extract' @Integer =<< pop
    let val = VAtom (EInt (t2 * t1))
    stack %= (val :)

loeE :: Eval ()
loeE = do
    t1 <- extract' @Integer =<< pop
    t2 <- extract' @Integer =<< pop
    let val = VAtom (EInt (if t2 <= t1 then 1 else 0))
    stack %= (val :)

printE :: Eval ()
printE =
    embed . print =<< pop