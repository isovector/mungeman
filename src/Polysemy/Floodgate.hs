{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Floodgate where

import GHC.Types
import Control.Monad
import Polysemy
import Polysemy.State
import Unsafe.Coerce

data Floodgate m a where
  Hold :: m () -> Floodgate m ()
  Release :: Floodgate m ()

makeSem ''Floodgate


runFloodgate
    :: forall r a
     . Sem (Floodgate ': r) a
    -> Sem r a
runFloodgate = fmap snd . runState @[Any] [] . reinterpretH
  ( \case
      Hold m -> do
        m' <- fmap void $ runT m
        -- These Anys are here because the monadic action references 'r', and
        -- if we exposed that, 'r' would be an infinite type
        modify (unsafeCoerce @_ @Any (raise $ runFloodgate m') :)
        getInitialStateT

      Release -> do
        ms' <- gets (fmap unsafeCoerce . reverse)
        sequence_ ms'
        getInitialStateT
  )

