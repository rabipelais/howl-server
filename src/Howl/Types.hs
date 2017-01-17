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
data FollowStatus = Pending | Accepted | Ignored | Blocked
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
