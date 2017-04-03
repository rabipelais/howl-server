{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Howl.Notifications (
  sendFollowTask,
  notificationsService,
  NotificationEnv(..),
  NotificationM
) where

import Prelude as P
import qualified Howl.Facebook              as Fb

import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Text as T
import           GHC.Generics

import           Database.Esqueleto           (from, select, (^.))
import qualified Database.Esqueleto           as E
import           Database.Persist
import           Database.Persist.Sql

import           Control.Lens.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader


import           Control.Concurrent (threadDelay)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Monoid ((<>))

import           Network.HTTP.Nano
import           Network.AMQP

import           Data.Aeson

import           Howl.Message
import           Howl.Models
import           Howl.Monad hiding (asks)
import           Howl.Types
import           Howl.Queue
import           Howl.Utils

import qualified Database.Firebase          as F

type NotificationM = ReaderT NotificationEnv (ExceptT HttpError IO)

data NotificationEnv = NotificationEnv {
  notifFirebase :: F.Firebase,
  notifHttp     :: HttpCfg,
  notifPool :: ConnectionPool
}

instance HasHttpCfg NotificationEnv where
  httpCfg = lens notifHttp (\te h -> te { notifHttp = h })

instance F.HasFirebase NotificationEnv where
  firebase = lens notifFirebase (\te f -> te { notifFirebase = f })

deriving instance (Show a) => Show (F.Message a)
deriving instance (Show a) => Show (F.MessageBody a)
deriving instance Show F.Priority

createMessage endpoints payload =
  F.Message
  Nothing -- to
  endpoints -- registrationIDs
  Nothing -- collapseKeys
  F.High -- Priority
  Nothing -- content_available
  Nothing -- delay_while_idle (deprecated)
  Nothing -- time_to_live
  (F.Data payload) -- data

sendNotification :: Notification -> NotificationM ()
sendNotification = send
  where
    send (FollowNotification (FollowN source target)) = do
      (endpoints, sourceUser) <- runQuery $ do
        eEntities <- selectList [DeviceUserId ==. target] []
        (Just (Entity k u)) <- getBy $ UniqueUserID source
        return (P.map entityVal eEntities, u)
      let
        payload = object [ "type" .= ("follow" :: String)
                         , "source" .= source
                         , "sourceName" .= userFirstName sourceUser
                         , "target" .= target]
        endpoints' = (P.map (T.unpack . deviceDeviceId) endpoints)
        msg = createMessage endpoints' payload
      liftIO $ print endpoints'
      liftIO $ print payload
      F.sendMessage $ msg
    send _ = liftIO $ putStrLn "Unknown notification type"

notificationsService env conn ch qName = do
  declareQueue ch newQueue{queueName       = qName,
                            queueAutoDelete = False,
                            queueDurable    = True}

  qos ch 0 1 False

  BL.putStrLn " [*] Waiting for messages. To exit press CTRL+C"
  consumeMsgs ch qName Ack (deliveryHandler env)
  loop $ while True
  where
    loop l = body
      where
        body = do x <-  l
                  when (isJust x) body
    while b = return $ if b then Just () else Nothing
    isJust (Just _) = True
    isJust _ = False

deliveryHandler :: NotificationEnv -> (Message, Envelope) -> IO ()
deliveryHandler env (msg, metadata) = do
  BL.putStrLn $ " [x] Received " <> body
  putStrLn $ "  [x] As JSON: " <> (show notification)
  case notification of
    Nothing -> putStrLn "  [x] Couldn't decode payload"
    Just n -> (runExceptT $ flip runReaderT env (sendNotification n)) >>= \case
      Left e -> error $ show e
      Right r -> return ()
  BL.putStrLn " [x] Done"
  ackEnv metadata
  where
    body = msgBody msg
    notification :: Maybe Notification = decode body
    n = countDots body

countDots :: BL.ByteString -> Int
countDots = fromIntegral . BL.count '.'

sendFollowTask ch source target = do
  publishMsg ch "" (T.pack $ show NotificationsTask)
    (newMsg {msgBody         = body,
             msgDeliveryMode = Just Persistent})
  where
    body = encode (FollowNotification $ FollowN source target)


data Notification =
  FollowNotification FollowN
  | EventNotification EventN
  deriving (Eq, Show, Generic)

deriving instance ToJSON Notification
deriving instance FromJSON Notification

data FollowN = FollowN { followSourceId :: IDType
                       , followTargetId :: IDType}
  deriving (Eq, Show, Generic)

deriving instance ToJSON FollowN
deriving instance FromJSON FollowN

data EventN = EventN { eventSourceId :: IDType
                     , eventTargetId :: IDType
                     , eventID :: IDType}
  deriving (Eq, Show, Generic)

deriving instance ToJSON EventN
deriving instance FromJSON EventN

runQuery query = do
  pool <- asks notifPool
  (runSqlPool query pool)

runQuery' query pool = do
  (runSqlPool query pool)
