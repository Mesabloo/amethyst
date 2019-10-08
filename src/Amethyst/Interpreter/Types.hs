{-# LANGUAGE DataKinds, GADTs, TypeApplications, ScopedTypeVariables
           , FlexibleInstances, TemplateHaskell, MultiParamTypeClasses
           , FlexibleContexts, UndecidableInstances, TypeOperators #-}

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

type Sem' = Sem '[Error EvalError, State EvalState, Embed IO]
type Eval a = Block -> Sem' a

data Value
    = VAtom Lit
    | VId Id
    | VBlock Block
    | VNative (Sem' Value)

data EvalState = EvalState
    { _stack :: [Value]
    , _env :: Map.Map Text.Text Value }
makeLenses ''EvalState

-----------------------------------------------------------------------

class Typeable a => Extractable a where
    {-# MINIMAL extract #-}
    extract :: Value -> Sem' a

    error' :: Value -> Sem' a
    error' v = throw @EvalError (show v <> " is not a " <> show (typeRep (Proxy @a)))

instance Extractable Integer where
    extract (VAtom (EInt i)) = pure i
    extract v = error' v

instance Extractable Text.Text where
    extract (VAtom (EString s)) = pure s
    extract v = error' v

instance Extractable Id where
    extract (VId i) = pure i
    extract v = error' v

instance Extractable Block where
    extract (VBlock vs) = pure vs
    extract v = error' v

instance Extractable Char where
    extract (VAtom (EChar c)) = pure c
    extract v = error' v

instance Extractable Double where
    extract (VAtom (EFloat f)) = pure f
    extract v = error' v

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