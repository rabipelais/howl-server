{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Howl.Notifications where

import qualified Howl.Facebook              as Fb

import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Text as T
import           GHC.Generics

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
import           Howl.Monad
import           Howl.Types
import           Howl.Queue
import           Howl.Utils

import qualified Database.Firebase          as F

type NotificationM = ReaderT NotificationEnv (ExceptT HttpError IO)

data NotificationEnv = NotificationEnv {
  notifFirebase :: F.Firebase,
  notifHttp     :: HttpCfg
}

instance HasHttpCfg NotificationEnv where
  httpCfg = lens notifHttp (\te h -> te { notifHttp = h })

instance F.HasFirebase NotificationEnv where
  firebase = lens notifFirebase (\te f -> te { notifFirebase = f })

deriving instance (Show a) => Show (F.Message a)
deriving instance (Show a) => Show (F.MessageBody a)
deriving instance Show F.Priority

createMessage endpoint payload =
  F.Message
  Nothing -- to
  [endpoint] -- registrationIDs
  Nothing -- collapseKeys
  F.High -- Priority
  Nothing -- content_available
  Nothing -- delay_while_idle (deprecated)
  Nothing -- time_to_live
  (F.Data payload) -- data

sendNotification :: (ToJSON a, Show a) => String -> a -> NotificationM ()
sendNotification endpoint payload = do
  let msg = createMessage endpoint payload
  F.sendMessage $ msg


notificationsService conn ch qName = do
  declareQueue ch newQueue{queueName       = qName,
                            queueAutoDelete = False,
                            queueDurable    = True}

  qos ch 0 1 False

  BL.putStrLn " [*] Waiting for messages. To exit press CTRL+C"
  consumeMsgs ch qName Ack deliveryHandler

  -- waits for keypresses
  getLine
  closeConnection conn

deliveryHandler :: (Message, Envelope) -> IO ()
deliveryHandler (msg, metadata) = do
  BL.putStrLn $ " [x] Received " <> body
  putStrLn $ "  [x] As JSON: " <> (show jsonBody)
  threadDelay (1000000 * n)
  BL.putStrLn " [x] Done"
  ackEnv metadata
  where
    body = msgBody msg
    jsonBody :: Maybe Notification = decode body
    n    = countDots body

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
