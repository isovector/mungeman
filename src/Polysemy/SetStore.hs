{-# LANGUAGE TemplateHaskell #-}

module Polysemy.SetStore where

import           Control.Monad
import           Data.Binary (Binary)
import           Data.Foldable
import qualified Data.Set as S
import qualified Database.Redis as R
import           Polysemy
import           Polysemy.Error
import           Polysemy.KVStore
import           Polysemy.RedisUtils


data SetStore k v m a where
  AddS :: k -> v -> SetStore k v m ()
  DelS :: k -> v -> SetStore k v m ()
  MemberS :: k -> v -> SetStore k v m Bool

makeSem ''SetStore

type InterpreterOf e r = forall x.  Sem (e ': r) x -> Sem r x


runSetStoreAsKVStore
    :: ( Member (KVStore k (S.Set v)) r
       , Ord v
       )
    => InterpreterOf (SetStore k v) r
runSetStoreAsKVStore = interpret $ \case
  AddS k v ->
    lookupKV k >>= \case
      Just s  -> writeKV k $ S.insert v s
      Nothing -> writeKV k $ S.singleton v
  DelS k v -> do
    ms <- lookupKV k
    for_ ms $ writeKV k . S.delete v
  MemberS k v ->
    pure . maybe False (S.member v) =<< lookupKV k


runSetStoreInRedis
    :: ( Member (Lift R.Redis) r
       , Member (Error R.Reply) r
       , Binary k
       , Binary v
       )
    => (k -> Path)
    -> InterpreterOf (SetStore k v) r
runSetStoreInRedis pf = interpret $ \case
  AddS k v -> void
            . sendMError
            . R.sadd (getPath $ pf k)
            . pure
            $ putForRedis v
  DelS k v -> void
            . sendMError
            . R.srem (getPath $ pf k)
            . pure
            $ putForRedis v
  MemberS k v -> sendMError
               . R.sismember (getPath $ pf k)
               $ putForRedis v

