{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

-- This is mostly for the TH staging restrictions
module Types where

import Database.Persist.TH
import Data.Aeson

import Prelude
import GHC.Generics

data RSVP = Attending | Created | Declined
          | Maybe | NotReplied
          deriving (Show, Read, Eq, Generic)

instance ToJSON RSVP where
  -- For efficiency, we write a simple toEncoding implementation, as
  -- the default version uses toJSON.
  toEncoding = genericToEncoding defaultOptions

instance FromJSON RSVP
derivePersistField "RSVP"




data FriendshipStatus = Pending | Accepted | Ignored | Blocked
          deriving (Show, Read, Eq, Generic)

instance ToJSON FriendshipStatus where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON FriendshipStatus
derivePersistField "FriendshipStatus"
