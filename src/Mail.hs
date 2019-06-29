{-# LANGUAGE OverloadedStrings #-}

module Mail where

import           Control.Monad
import           Data.ByteString (ByteString)
import           Data.Function
import           Data.String.Conv (toS)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Mail.Hailgun
import           Model.Bookmark
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input
import           Polysemy.KVStore
import           Polysemy.Output
import           Polysemy.Warning

newtype MissingCustomer = MissingCustomer CustomerKey
  deriving Show


mkSubject :: Day -> Text
mkSubject day = T.pack $ "Mungeman Digest for " ++ show day


mkPost :: Post -> ByteString
mkPost p = toS $ unlines
  [ "* " ++ show (pBookmark p)
  , toS $ pContent p
  , ""
  , replicate 80 '-'
  , ""
  , ""
  ]


unsafeHailgunMessage
    :: MessageSubject
    -> MessageContent
    -> UnverifiedEmailAddress
    -> MessageRecipients
    -> [Attachment]
    -> HailgunMessage
unsafeHailgunMessage a b c d e
  = either (error . show) id $ hailgunMessage a b c d e


buildEmailFromDigest
    :: Members '[ Error HailgunErrorMessage
                , Error MissingCustomer
                , KVStore CustomerKey Customer
                , Input Day
                ] r
    => Digest
    -> Sem r HailgunMessage
buildEmailFromDigest (Digest ck ps) = do
  c <- lookupOrThrowKV MissingCustomer ck
  now <- input

  let content = "New content:\n" <> mconcat (fmap mkPost ps)

  fromEither $
    hailgunMessage
      (mkSubject now)
      (TextOnly content)
      "sandy@sandymaguire.me"
      emptyMessageRecipients { recipientsTo = cEmails c }
      []


sendDigests
    :: Members '[ Lift IO
                , Error HailgunErrorMessage
                , Error HailgunErrorResponse
                , Error MissingCustomer
                , KVStore CustomerKey Customer
                , Input Day
                ] r
    => HailgunContext
    -> Sem (Output Digest ': r) a
    -> Sem r a
sendDigests ctx = interpret $ \case
  Output d -> do
    e <- buildEmailFromDigest d
    void $ fromEitherM $ sendEmail ctx e

getCurrentDay :: IO Day
getCurrentDay = do
    t <- getZonedTime
    return $ localDay (zonedTimeToLocalTime t)


reportWarning
    :: ( Member (Lift IO) r
       )
    => HailgunContext
    -> UnverifiedEmailAddress
    -> Sem (Warning ': r) ()
    -> Sem r ()
reportWarning ctx email m
  = m
  & interpret (
      \case
        EmitWarning w msg -> do
          void $ sendM $ sendEmail ctx $
            unsafeHailgunMessage
              (toS $ show w)
              (TextOnly $ toS msg)
              "sandy@sandymaguire.me"
              emptyMessageRecipients { recipientsTo = [email] }
              []
    )

