{-# LANGUAGE DeriveGeneric #-}

module Howl.Notifications where

import qualified Howl.Facebook              as Fb

import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Text
import           GHC.Generics

import           Control.Lens.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader

import           Network.HTTP.Nano

import           Data.Aeson

import           Howl.Message
import           Howl.Models
import           Howl.Monad
import           Howl.Types
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
  liftIO $ print $ encode msg
  F.sendMessage $ msg
