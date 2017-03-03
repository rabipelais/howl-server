{-# LANGUAGE OverloadedStrings #-}
module Howl.Message where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import           Network.AMQP


message chan queue obj = messageExchange chan "" queue obj

messageExchange chan exchange routingKey obj = liftIO $ publishMsg chan exchange routingKey (newMsg{msgBody = encode obj, msgDeliveryMode = Just Persistent})
