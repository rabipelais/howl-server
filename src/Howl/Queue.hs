module Howl.Queue
  ( Settings(..)
  , withConnection
  , Queues (..)
  , module Export) where

import           Control.Exception.Base (bracket)
import           Data.Text
import           Network.AMQP           as Export

data Settings = Settings
  { host     :: String
  , vhost    :: Text
  , user     :: Text
  , password :: Text
  }

withConnection (Settings host vhost user password) queueOpts action =
  bracket setup cleanup $ \(conn, chan) -> action (conn, chan)
  where
    setup = do
      conn <- openConnection host vhost user password
      chan <- openChannel conn
      qos chan 0 1 False
      queues <- mapM (declareQueue chan) queueOpts
      putStrLn "Declaring following queues:..."
      mapM (putStrLn . show) queues
      return (conn, chan)
    cleanup (conn, chan) = closeConnection conn

data Queues = NotificationsTask

instance Show Queues where
  show NotificationsTask = "notifications_task"
