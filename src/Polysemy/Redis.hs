module Polysemy.Redis where

import           Control.Monad.IO.Class
import qualified Database.Redis as R
import           Polysemy
import           Polysemy.Internal
import           Polysemy.Internal.Union


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

