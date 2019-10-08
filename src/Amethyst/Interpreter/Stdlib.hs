{-# LANGUAGE LambdaCase, TypeApplications, OverloadedStrings #-}

module Amethyst.Interpreter.Stdlib where

import qualified Data.Map as Map
import qualified Data.Text as Text
import Amethyst.Interpreter.Types
import Amethyst.Language.Parser
import Polysemy.Error
import Polysemy.State
import Debug.Trace

defaultEnv :: Map.Map Text.Text Value
defaultEnv = Map.fromList
    [ ("+", VNative addE)
    , ("=", VNative assE)
    , ("-", VNative subE)
    , ("swap", VNative swapE) ]

addE :: Sem' Value
addE = gets _stack >>= \case
    t1:t2:xs -> do
        i1 <- extractInt t1
        i2 <- extractInt t2
        let val = VAtom (EInt (i2 + i1))
        (val <$) . modify $ \st -> st { _stack = val : xs }
    s       -> throw @EvalError ("Not enough arguments for calling `+`: " <> show s)

assE :: Sem' Value
assE = gets _stack >>= \case
    e1:name:xs -> do
        i <- extractId name
        (e1 <$) . modify $ \st -> st { _stack = xs, _env = Map.insert i e1 (_env st) }
    s       -> throw @EvalError ("Not enough arguments for calling `=`: " <> show s)

subE :: Sem' Value
subE = gets _stack >>= \case
    t1:t2:xs -> do
        i1 <- extractInt t1
        i2 <- extractInt t2
        let val = VAtom (EInt (i2 - i1))
        (val <$) . modify $ \st -> st { _stack = val : xs }
    s       -> throw @EvalError ("Not enough arguments for calling `-`: " <> show s)

swapE :: Sem' Value
swapE = gets _stack >>= \case
    t1:t2:xs ->
        (t1 <$) . modify $ \st -> st { _stack = t2:t1:xs }
    s -> throw @EvalError ("Not enough arguments for calling `swap`: " <> show s)

extractInt :: Value -> Sem' Integer
extractInt (VAtom (EInt i)) = pure i
extractInt v = throw @EvalError (show v <> " is not an integer.")

extractId :: Value -> Sem' Text.Text
extractId (VId name) = pure name
extractId v = throw @EvalError (show v <> " is not an identifier.")

extractBlock :: Value -> Sem' [Expr]
extractBlock (VBlock vs) = pure vs
extractBlock v = throw @EvalError (show v <> " is not a code block.")