{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

-- This is mostly for the TH staging restrictions
module Howl.Types where

import           Data.Aeson
import           Data.Text
import           Database.Persist.TH
import           GHC.Generics
import           Prelude

import qualified Howl.Facebook       as FB
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
  toEncoding = genericToEncoding defaultOptions

instance FromJSON DeviceType
derivePersistField "DeviceType"
