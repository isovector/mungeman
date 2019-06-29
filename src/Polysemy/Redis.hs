module Polysemy.Redis where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Database.Redis as R
import           Polysemy
import           Polysemy.Error
import           Polysemy.Internal
import           Polysemy.Internal.Union
import           Polysemy.Redis.Utils
import           Polysemy.State


runRedis
    :: LastMember (Lift IO) r
    => R.ConnectInfo
    -> Sem (Lift R.Redis ': r) a
    -> Sem r a
runRedis config (Sem m) = withLowerToIO $ \lower _ -> do
  conn <- R.checkedConnect config
  res <- R.runRedis conn $ do
    m $ \u ->
      case decomp u of
        Left x -> liftIO
                . lower
                . liftSem
                $ hoist (runRedis config) x

        Right (Yo (Lift wd) s _ y _) ->
          fmap y $ fmap (<$ s) wd
  R.disconnect conn
  pure res


runStateInRedis
    :: ( Member (Lift R.Redis) r
       , Member (Error R.Reply) r
       , Binary s
       )
    => s  -- ^ Default value
    -> Path
    -> Sem (State s ': r) a
    -> Sem r a
runStateInRedis s (Path p) = interpret $ \case
  Get -> do
    res <- fromEitherM $ R.get p
    pure $ maybe s id $ fmap getFromRedis res

  Put s' ->
    void . fromEitherM
         . R.set p
         $ putForRedis s'
{-# INLINE runStateInRedis #-}


