{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import Amethyst.Language.Parser
import Amethyst.Interpreter.Evaluator
import qualified Text.Megaparsec as Mega
import Control.Monad

main :: IO ()
main = case Mega.runParser program "test" "\\f { \\a 3 = } = f a" of
    Left e -> putStrLn (Mega.errorBundlePretty e)
    Right p ->
        runEval (eval p) >>= \(s, x) -> print s *> case x of
            Left e -> putStrLn e
            Right x -> print x