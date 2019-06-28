{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Web where

import Control.Monad
import Data.Traversable
import Control.Monad.IO.Class
import Polysemy
import Polysemy.Internal
import Polysemy.Internal.Union
import Test.WebDriver


runWeb
    :: LastMember (Lift IO) r
    => WDConfig
    -> Sem (Lift WD ': r) a
    -> Sem r a
runWeb config (Sem m) = withLowerToIO $ \lower _ -> do
  runSession config $ do
    a <- m $ \u ->
      case decomp u of
        Left x -> liftIO
                . lower
                . liftSem
                $ hoist (runWeb config) x

        Right (Yo (Lift wd) s _ y _) ->
          fmap y $ fmap (<$ s) wd
    closeSession
    pure a

following
    :: Member (Lift WD) r
    => Selector
    -> (Element -> Sem r a)
    -> Sem r [a]
following sel f = do
  first_found <- sendM $ findElems sel
  for [0 .. length first_found - 1] $ \n -> do
    first_page <- sendM getCurrentURL
    el <- sendM $ (!! n) <$> findElems sel
    res <- f el
    cur_page <- sendM $ getCurrentURL
    when (first_page /= cur_page) $ sendM back
    pure res

