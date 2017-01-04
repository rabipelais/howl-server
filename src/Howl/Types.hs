{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

-- This is mostly for the TH staging restrictions
module Howl.Types where

import Database.Persist.TH
import Data.Aeson
import Data.Text
import GHC.Generics
import qualified Howl.Facebook as FB
import Prelude

import Servant.API

data FriendshipStatus = Pending | Accepted | Ignored | Blocked
  deriving (Show, Read, Eq, Generic)

instance ToJSON FriendshipStatus where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON FriendshipStatus
derivePersistField "FriendshipStatus"

data Authenticated a = Authenticated { auth :: FB.UserAccessToken
                                     , content :: a}
  deriving (Show, Eq, Generic)

instance (ToJSON a) => ToJSON (Authenticated a) where
    toEncoding = genericToEncoding defaultOptions

instance (FromJSON a) => FromJSON (Authenticated a)

type Token = Text


instance ToHttpApiData FB.Id where
  toUrlPiece = FB.idCode

instance FromHttpApiData FB.Id where
  parseUrlPiece = Right . FB.Id
