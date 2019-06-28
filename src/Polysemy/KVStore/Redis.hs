{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Polysemy.KVStore.Redis
  ( Path (..)
  , runKVStoreInRedis
  ) where

import           Control.Monad
import           Data.Binary (Binary)
import qualified Data.Binary as B
import           Data.Binary.Get (runGet)
import           Data.Binary.Put (runPut)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Database.Redis as R
import           GHC.Exts
import           Polysemy
import           Polysemy.Error
import           Polysemy.KVStore


newtype Path = Path { getPath :: ByteString }
  deriving (Eq, Ord, Show, IsString)


-- TODO(sandy): base lib
hoistError
    :: Member (Error e) r
    => Either e a
    -> Sem r a
hoistError (Left e) = throw e
hoistError (Right a) = pure a

sendMError
    :: ( Member (Error e) r
       , Member (Lift m) r
       )
    => m (Either e a)
    -> Sem r a
sendMError = hoistError <=< sendM


putForRedis :: Binary a => a -> ByteString
putForRedis = L.toStrict . runPut . B.put

getFromRedis :: Binary a => ByteString -> a
getFromRedis = runGet B.get . L.fromStrict


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

