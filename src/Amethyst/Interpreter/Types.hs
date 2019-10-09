{-# LANGUAGE DataKinds, GADTs, TypeApplications, ScopedTypeVariables
           , FlexibleInstances, TemplateHaskell, MultiParamTypeClasses
           , FlexibleContexts, UndecidableInstances, TypeOperators, LambdaCase #-}

module Amethyst.Interpreter.Types where

import Polysemy
import Polysemy.Error
import Polysemy.State
import Polysemy.IO
import qualified Data.Text as Text
import qualified Data.Map as Map
import Amethyst.Language.Parser
import Data.Typeable
import Control.Lens
import Control.Monad.State (MonadState(..))

type EvalError = String
type Block = [Expr]
newtype Id = Id Text.Text

data Value
    = VAtom Lit
    | VId Id
    | VBlock Block
    | VNative (Sem' Value)

data EvalState = EvalState
    { _stack :: [Value]
    , _env :: Map.Map Text.Text Value }

type Effects = '[Error EvalError, State EvalState, Embed IO]
type Sem' = Sem Effects
type Eval a = Block -> Sem' a

makeLenses ''EvalState
makePrisms ''Value

-----------------------------------------------------------------------

class Extractable a where
    extract :: Prism' Value a

extract' :: forall a. (Extractable a, Typeable a) => Value -> Sem' a
extract' v = case v ^? extract of
    Just x  -> pure x
    Nothing -> throw @EvalError (show v <> " is not a " <> show (typeRep (Proxy @a)))

instance Extractable Integer where
    extract v = v & (_VAtom . _EInt)

instance Extractable Text.Text where
    extract v = v & (_VAtom . _EString)

instance Extractable Id where
    extract v = v & _VId

instance Extractable Block where
    extract v = v & _VBlock

instance Extractable Char where
    extract v = v & (_VAtom . _EChar)

instance Extractable Double where
    extract v = v & (_VAtom . _EFloat)

--------------------------------------------------------------------------------

instance Show Value where
    show (VNative _) = ""
    show (VBlock []) = "{}"
    show (VBlock (v:vs)) = "{ " <> foldl ((. show) . (<>) . (<> ", ")) (show v) vs <> " }"
    show (VId (Id name)) = Text.unpack name
    show (VAtom l) = showLit l
      where showLit (EInt i) = show i
            showLit (EFloat f) = show f
            showLit (EString s) = show s
            showLit (EChar c) = show c

instance Show EvalState where
    show (EvalState st e) = show st <> "\n" <> show e

--------------------------------------------------------------------------------

instance MonadState EvalState Sem' where
    get = Polysemy.State.get
    put = Polysemy.State.put