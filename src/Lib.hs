{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Lib where

import           App
import           Data.Function
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.String.Conv
import qualified Database.Redis as R
import           Mail
import           Model.Bookmark
import           Polysemy
import           Polysemy.Alias
import           Polysemy.Error
import           Polysemy.Fetch
import           Polysemy.Input
import           Polysemy.KVStore
import           Polysemy.Output
import           Polysemy.Redis
import           Polysemy.Redis.Utils
import           Polysemy.SetStore
import           Polysemy.State
import           Polysemy.Trace
import           Polysemy.Warning
import           Polysemy.Web
import           Test.WebDriver

import Mail.Hailgun


chromeConfig :: WDConfig
chromeConfig =
  useBrowser
    chrome -- { chromeOptions = ["--headless"] }
    defaultConfig

evalState :: s -> InterpreterOf (State s) r
evalState s = fmap snd . runState s


bookmarks :: [Bookmark]
bookmarks =
  [ Bookmark
      0
      "https://mbasic.facebook.com/leCDEA/"
      Facebook
  -- , Bookmark
  --     0
  --     "https://mbasic.facebook.com/We-Can-Solve-This-403614660395082/"
  --     Facebook
  ]

debugBookmarks :: M.Map BookmarkKey [Post]
debugBookmarks = M.fromList
  [ let key =  "https://mbasic.facebook.com/Reasonably-Polymorphic-2174434622886729/"
     in ( key
        , [ Post 0 "b1-p1" key "now" "b1-p1"
          , Post 0 "b1-p2" key "now" "b1-p2"
          ]
        )
  , let key =  "https://mbasic.facebook.com/We-Can-Solve-This-403614660395082/"
     in ( key
        , [ Post 0 "b2-p1" key "now" "b2-p1"
          , Post 0 "b2-p2" key "now" "b2-p2"
          ]
        )
  ]


debugKeys :: [PostKey]
debugKeys =
  [ "https://mbasic.facebook.com/story.php?story_fbid=2232120570451467&id=2174434622886729"
  , "https://mbasic.facebook.com/story.php?story_fbid=2233848760278648&id=2174434622886729"
  , "https://mbasic.facebook.com/story.php?story_fbid=2237959419867582&id=2174434622886729"
  , "https://mbasic.facebook.com/story.php?story_fbid=2257002114629979&id=2174434622886729"
  , "https://mbasic.facebook.com/story.php?story_fbid=2257891424541048&id=2174434622886729"
  ]

customers :: [Customer]
customers =
  [ Customer 0 "sandy" ["sandy@sandymaguire.me"]
  ]



saveOutputToKVStore
    :: ( Members '[ Output v
                  , KVStore k v
                  , Trace
                  ] r
       , Show k
       )
    => (v -> k)
    -> Sem r a
    -> Sem r a
saveOutputToKVStore f = intercept $ \case
  Output v -> do
    let k = f v
    trace $ "saving " ++ show k
    writeKV k v
    output v


runKVAndKeyState
    :: Ord k
    => (v -> k)
    -> [v]
    -> Sem (State [k] ': KVStore k v ': r) a
    -> Sem r a
runKVAndKeyState key_of vs
  = evalKVStorePurely (M.fromList $ fmap (\v -> (key_of v, v)) vs)
  . evalState (fmap key_of vs)

runUnsafeError :: Show e => Sem (Error e ': r) a -> Sem r a
runUnsafeError = fmap (either (error . show) id)
               . runError


main :: IO ()
main = do
  let ctx = HailgunContext
              "<domain>"
              "<password>"
              Nothing

  (loadAllBookmarks >> output (SendEvent "sandy"))
    & evalState bookmarks
    & saveOutputToKVStore pUrl
    & pubSub
    & sendDigests ctx

    & runKVStoreInRedis @_ @[Post]
        (\(CustomerKey ck) -> Path $ toS $ "customer." ++ ck ++ ".digest")
    & runSetStoreAsKVStore @CustomerKey @BookmarkKey
    & evalKVStorePurely @CustomerKey @(S.Set BookmarkKey)
        (M.singleton "sandy" $ S.fromList $ fmap bmUrl bookmarks)
    & runKVStoreInRedis @_ @Post
        (\(PostKey pk) -> Path $ toS $ "posts." ++ pk)

    & runErrorAsWarning @R.Reply              Fatal   show
    & runErrorAsWarning @HailgunErrorResponse Warning show
    & runErrorAsWarning @HailgunErrorMessage  Info    show
    & runErrorAsWarning @MissingCustomer      Warning show
    & reportWarning ctx "sandy@sandymaguire.me"

    & runMonadicInput (sendM getCurrentDay)
    & runKVAndKeyState @CustomerKey @Customer
        cKey
        customers

    & runFetchAsWD
    & runWeb chromeConfig
    & runRedis R.defaultConnectInfo
    & runTraceIO
    & runM

