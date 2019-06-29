{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Polysemy.Fetch where

import           Data.List
import           Data.Maybe
import qualified Data.Text as T
import           Model.Bookmark
import           Polysemy
import           Polysemy.KVStore
import           Polysemy.Web
import           Test.WebDriver


data Fetch m a where
  FetchBookmark :: Bookmark -> Fetch m [PostKey]
  FetchPost :: BookmarkKey -> PostKey -> Fetch m Post

makeSem ''Fetch


cleanUpFacebookKey :: String -> String
cleanUpFacebookKey = dropSentinel "&refid"


dropSentinel :: Eq a => [a] -> [a] -> [a]
dropSentinel _ []          = []
dropSentinel sent as
  | isPrefixOf sent as     = []
dropSentinel sent (a : as) = a : dropSentinel sent as


------------------------------------------------------------------------------
-- | maybe we can just use '.gu'
runFetchAsWD
    :: Member (Lift WD) r
    => Sem (Fetch ': r) a
    -> Sem r a
runFetchAsWD = interpret $ \case
  FetchBookmark (Bookmark _ (BookmarkKey url) Facebook) -> do
    sendM $ openPage url
    following (ByPartialLinkText "Full Story") $ \a -> do
      mhref <- sendM $ attr a "href"
      let Just href = mhref
          key = PostKey $ cleanUpFacebookKey $ T.unpack href
      pure key

  FetchPost bKey key@(PostKey url) -> do
    sendM $ openPage url
    story  <- fmap (take 1) $ sendM $ findElems $ ByCSS "#m_story_permalink_view .bx"
    timeEl <- sendM $ findElem $ ByCSS "abbr"
    text <- sendM $ traverse getText story
    time <- sendM $ getText timeEl
    pure $ Post 0 key bKey time $ mconcat text


runFetchAsKVStore
    :: Member (KVStore BookmarkKey [Post]) r
    => Sem (Fetch ': r) a
    -> Sem r a
runFetchAsKVStore = interpret $ \case
  FetchBookmark (Bookmark _ b _) ->
    fmap pUrl . fromJust <$> lookupKV b

  FetchPost b p -> fromJust
                 . find ((== p) . pUrl)
                 . fromJust
               <$> lookupKV b

