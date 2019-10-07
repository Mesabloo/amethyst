{-# LANGUAGE DataKinds, GADTs #-}

module Amethyst.Interpreter.Types where

import Polysemy
import Polysemy.Error
import Polysemy.State
import Polysemy.IO
import qualified Data.Text as Text
import qualified Data.Map as Map
import Amethyst.Language.Parser

data EvalState = EvalState
    { _stack :: [Value]
    , _env :: Map.Map Text.Text Value }

data Value where
    VAtom :: Lit -> Value
    VId :: Text.Text -> Value
    VBlock :: [Expr] -> Value
    VNative :: Sem' Value -> Value

type EvalError = String

type Sem' = Sem '[Error EvalError, State EvalState, Embed IO]
type Eval a = [Expr] -> Sem' a

-----------------------------------------------------------------------

instance Show Value where
    show (VNative _) = ""
    show (VBlock []) = "{}"
    show (VBlock (v:vs)) = "{ " <> foldl ((. show) . (<>) . (<> ", ")) (show v) vs <> " }"
    show (VId name) = Text.unpack name
    show (VAtom l) = showLit l
      where showLit (EInt i) = show i
            showLit (EFloat f) = show f
            showLit (EString s) = show s
            showLit (EChar c) = show c

instance Show EvalState where
    show (EvalState st e) = show st <> "\n" <> show e