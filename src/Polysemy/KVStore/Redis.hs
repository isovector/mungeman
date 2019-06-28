{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Polysemy.KVStore.Redis
  ( Path (..)
  , runKVStoreInRedis
  ) where

import           Control.Monad
import           Data.Binary (Binary)
import qualified Database.Redis as R
import           Polysemy
import           Polysemy.Error
import           Polysemy.KVStore
import           Polysemy.RedisUtils


runKVStoreInRedis
    :: ( Member (Lift R.Redis) r
       , Member (Error R.Reply) r
       , Binary k
       , Binary v
       )
    => (k -> Path)
    -> Sem (KVStore k v ': r) a
    -> Sem r a
runKVStoreInRedis pf = interpret $ \case
  LookupKV k -> do
    res <- sendMError $ R.hget (getPath $ pf k) $ putForRedis k
    pure $ fmap getFromRedis res

  UpdateKV k Nothing ->
    void . sendMError
         . R.hdel (getPath $ pf k)
         . pure
         $ putForRedis k

  UpdateKV k (Just v) ->
    void . sendMError
         . R.hset (getPath $ pf k) (putForRedis k)
         $ putForRedis v

