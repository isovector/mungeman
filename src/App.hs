module App
  ( SendEvent (..)
  , pubSub
  , loadAllBookmarks
  ) where

import Control.Monad
import Data.Foldable
import Data.Maybe
import Model.Bookmark
import Polysemy
import Polysemy.Fetch
import Polysemy.Trace
import Polysemy.KVStore
import Polysemy.Output
import Polysemy.SetStore
import Polysemy.State



newtype SendEvent = SendEvent CustomerKey

pubSub
    :: Members '[ KVStore CustomerKey [Post]
                , SetStore CustomerKey BookmarkKey
                , State [CustomerKey]
                , Output Digest
                ] r
    => Sem (Output Post ': Output SendEvent ': r) a
    -> Sem r a
pubSub = emitDigests
       . bucketOutput
       . routeSubscriptions pBookmark


loadAllBookmarks
    :: Members '[ Fetch
                , KVStore PostKey Post
                , Trace
                , Output Post
                , State [Bookmark]
                ] r
    => Sem r ()
loadAllBookmarks = do
  get >>= traverse_ loadBookmark


loadBookmark
    :: Members '[ Fetch
                , KVStore PostKey Post
                , Trace
                , Output Post
                ] r
    => Bookmark
    -> Sem r ()
loadBookmark b = do
  post_keys <- fetchBookmark b
  for_ post_keys $ \key ->
    existsKV key >>= \case
      True -> do
        trace $ "cached: " ++ show key
      False -> do
        trace $ "fetching: " ++ show key
        post <- fetchPost (bmUrl b) key
        output post


emitDigests
    :: Members '[ KVStore CustomerKey [Post]
                , Output Digest
                ] r
    => Sem (Output SendEvent ': r) a
    -> Sem r a
emitDigests = interpret $ \case
  Output (SendEvent ck) -> do
    mps <- lookupKV ck
    let ps = join $ maybeToList mps
    output $ Digest ck ps


routeSubscriptions
    :: ( Member (SetStore k j) r
       , Member (State [k]) r
       )
    => (o -> j)
    -> Sem (Output o ': r) a
    -> Sem (Output (k, o) ': r) a
routeSubscriptions f = reinterpret $ \case
  Output p -> do
    ks <- get
    for_ ks $ \k -> do
      mem <- memberS k $ f p
      when mem $ output (k, p)


bucketOutput
    :: Member (KVStore k [o]) r
    => Sem (Output (k, o) ': r) a
    -> Sem r a
bucketOutput = interpret $ \case
  Output (k, o) -> modifyKV [] (o :) k
