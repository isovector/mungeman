{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.Bookmark where

import Data.Binary
import GHC.Generics
import Data.Text (Text)
import GHC.Exts
import Data.ByteString (ByteString)

data Digest = Digest
  { dCustomer :: CustomerKey
  , dPosts :: [Post]
  }

newtype CustomerKey = CustomerKey
  { getCustomerKey :: String
  }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype IsString
  deriving anyclass Binary

data Customer = Customer
  { cVersion :: Int
  , cKey :: CustomerKey
  , cEmails :: [ByteString]
  }
  deriving (Eq, Ord, Show, Generic, Binary)

data BookmarkSource = Facebook
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Binary)

-- TODO(sandy): don't print mbasic
newtype BookmarkKey = BookmarkKey
  { getBookmarkKey :: String
  }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype IsString
  deriving anyclass Binary


data Bookmark = Bookmark
  { bmVersion :: Int
  , bmUrl    :: BookmarkKey
  , bmSource :: BookmarkSource
  }
  deriving (Eq, Ord, Show, Generic, Binary)


newtype PostKey = PostKey
  { getPostKey :: String
  }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype IsString
  deriving anyclass Binary


data Post = Post
  { pVersion  :: Int
  , pUrl      :: PostKey
  , pBookmark :: BookmarkKey
  , pTime     :: Text
  , pContent  :: Text
  }
  deriving (Eq, Ord, Show, Generic, Binary)

