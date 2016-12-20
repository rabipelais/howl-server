{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

-- This is mostly for the TH staging restrictions
module Howl.Types where

import Database.Persist.TH
import Data.Aeson
import GHC.Generics

import Prelude


data FriendshipStatus = Pending | Accepted | Ignored | Blocked
  deriving (Show, Read, Eq, Generic)

instance ToJSON FriendshipStatus where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON FriendshipStatus
derivePersistField "FriendshipStatus"
