{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Warning where

import Control.Monad
import Polysemy
import Polysemy.Error

data WarningLevel
  = Info
  | Warning
  | Fatal
  deriving (Eq, Ord, Show, Enum, Bounded)


data Warning m a where
  EmitWarning :: WarningLevel -> String -> Warning m ()

makeSem ''Warning


runErrorAsWarning
    :: forall e r
     . Member Warning r
    => WarningLevel
    -> (e -> String)
    -> Sem (Error e ': r) ()
    -> Sem r ()
runErrorAsWarning l f = either (emitWarning l . f) pure <=< runError


