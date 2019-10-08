{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import Amethyst.Language.Parser
import Amethyst.Interpreter.Evaluator
import qualified Text.Megaparsec as Mega
import Control.Monad

main :: IO ()
main = case Mega.runParser program "test" fib of
    Left e -> putStrLn (Mega.errorBundlePretty e)
    Right p ->
        runEval (eval p) >>= \(s, x) -> print s *> case x of
            Left e -> putStrLn e
            Right x -> print x

fib = "\\fib {dup 2 >= {} swap {dup 2 - fib swap 1 - fib +} swap ?:} = 2 fib"