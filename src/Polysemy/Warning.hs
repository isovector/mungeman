{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Warning where

import Control.Monad
import Polysemy
import Polysemy.Error
import Polysemy.Trace
import Data.Typeable

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
     . ( Typeable e
       , Members '[ Warning, Trace ] r
       )
    => WarningLevel
    -> (e -> String)
    -> Sem (Error e ': r) ()
    -> Sem r ()
runErrorAsWarning l f =
  let tyname = show $ typeRep $ Proxy @e
      f' e = "error (" ++ tyname ++ "): " ++ f e
   in either (\e -> trace (f' e) >> emitWarning l (f' e)) pure
        <=< runError


