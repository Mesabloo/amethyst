{-# LANGUAGE OverloadedStrings #-}

module Main where

import Amethyst.Language.Parser
import Amethyst.Interpreter.Evaluator
import qualified Text.Megaparsec as Mega
import Control.Monad
import Control.Lens
import Amethyst.Interpreter.Types

main :: IO ()
main = case Mega.runParser program "test" reduce of
    Left e -> putStrLn (Mega.errorBundlePretty e)
    Right p ->
        runEval (eval p) initEvalState
        >>= \(s, x) -> print (s ^. stack) *> case x of
            Left e -> putStrLn e
            Right x -> print x

fib = "\\fib {dup 2 >= {} swap {dup 2 - fib swap 1 - fib +} swap ?:} = 2 fib"
fact = "\\fact {dup 1 <= {pop 1} swap {dup 1 - fact *} swap ?:} = 5 fact"
reduce = "\\reduce {dup 1 <= {dup 1 - reduce} swap {0} swap ?:} = 10 reduce /"
