{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Polysemy.RedisUtils where

import           Control.Monad
import           Data.Binary (Binary)
import qualified Data.Binary as B
import           Data.Binary.Get (runGet)
import           Data.Binary.Put (runPut)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import           GHC.Exts
import           Polysemy
import           Polysemy.Error
import           Polysemy.KVStore


newtype Path = Path { getPath :: ByteString }
  deriving (Eq, Ord, Show, IsString)


lookupOrThrowKV
    :: Members '[ KVStore k v
                , Error e
                ] r
    => (k -> e)
    -> k
    -> Sem r v
lookupOrThrowKV f k =
  hoistError . maybe (Left $ f k) Right =<< lookupKV k

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

