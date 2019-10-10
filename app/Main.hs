{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import Amethyst.Language.Parser
import Amethyst.Interpreter.Evaluator
import qualified Text.Megaparsec as Mega
import Control.Monad
import Control.Lens
import Amethyst.Interpreter.Types
import Amethyst.REPL.REPL

main :: IO ()
main = runREPL launchREPL >>= \case
    Left x -> print x
    Right x -> pure ()

fib = "\\fib {dup 2 >= {} swap {dup 2 - fib swap 1 - fib +} swap ?:} = 2 fib"
fact = "\\fact {dup 1 <= {dup 1 - fact *} swap {pop 1} swap ?:} = 5 fact"
reduce = "\\reduce {dup 1 <= {dup 1 - reduce} swap {0} swap ?:} = 10 reduce"
