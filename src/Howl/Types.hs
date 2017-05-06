{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

-- This is mostly for the TH staging restrictions
module Howl.Types where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char
import           Data.Text              hiding (toLower)
import           Database.Persist.Class
import           Database.Persist.TH
import           Database.Persist.Types
import           GHC.Generics
import           Prelude

import qualified Howl.Facebook          as FB
import           Servant
import           Servant.API

-- FollowStatus
-- `None` is meant for people on Howl, so we can differentiate
-- with `Maybe Followstatus` Nothing people not on Howl
data FollowStatus = Pending | Accepted | Ignored | Blocked | None
  deriving (Show, Read, Eq, Generic)

instance ToJSON FollowStatus where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON FollowStatus
derivePersistField "FollowStatus"

type Token = Text

type IDType = FB.Id

instance ToHttpApiData FB.Id where
  toUrlPiece = FB.idCode

instance FromHttpApiData FB.Id where
  parseUrlPiece = Right . FB.Id

data DeviceType = Android | Iphone
  deriving (Show, Read, Eq, Generic)

instance ToJSON DeviceType where
  toJSON Android = String "android"
  toJSON Iphone = String "iphone"

instance FromJSON DeviceType where
  parseJSON (String "android") = return Android
  parseJSON (String "iphone") = return Iphone

derivePersistField "DeviceType"

instance ToHttpApiData DeviceType where
  toUrlPiece Android = "android"
  toUrlPiece Iphone = "iphone"

instance FromHttpApiData DeviceType where
  parseUrlPiece t = case t of
    "android" -> Right Android
    "iphone" -> Right Iphone
    otherwise -> Left "invalid DeviceType"
