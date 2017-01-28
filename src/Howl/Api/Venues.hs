{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Howl.Api.Venues where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Proxy
import           Data.Swagger               hiding (Header)
import           Data.Text
import           Data.Time
import           Data.Typeable              (Typeable)
import           GHC.Generics

import           Database.Persist
import qualified Howl.Facebook              as FB
import           Servant.Swagger            hiding (Header)

import           Howl.Api.Common
import           Howl.Models
import           Howl.Types

import           Servant.API

type VenuesAPI =
  VenuesGet
  :<|> VenuesPut
  :<|> VenuesNearbyGet
  :<|> VenuesIdGet
  :<|> VenuesIdFollowersGet
  :<|> VenuesIdFollowersIdGet
  :<|> VenuesIdFollowersIdPut
  :<|> VenuesIdFollowersIdDelete
  :<|> VenuesIdEventsGet

type Prefix = "venues"

type VenuesGet = Prefix :> Header "token" Token
               :> Get '[JSON] [Venue]

type VenuesPut = Prefix :> ReqBody '[JSON] Venue
               :> Header "token" Token
               :> Put '[JSON] Venue

type VenuesIdGet = Prefix :> Capture "venueID" IDType
                 :> Header "token" Token
                 :> Get '[JSON] Venue

type VenuesIdFollowersGet = Prefix :> Capture "venueID" IDType
                          :> "followers"
                          :> Header "token" Token
                          :> Get '[JSON] [User]

type VenuesIdFollowersIdGet = Prefix :> Capture "venueID" IDType
                              :> "followers"
                              :> Capture "userID" IDType
                              :> Header "token" Token
                              :> Get '[JSON] VenueFollower

type VenuesIdFollowersIdPut = Prefix :> Capture "venueID" IDType
                              :> "followers"
                              :> Capture "userID" IDType
                              :> Header "token" Token
                              :> Put '[JSON] VenueFollower

type VenuesIdFollowersIdDelete = Prefix :> Capture "venueID" IDType
                              :> "followers"
                              :> Capture "userID" IDType
                              :> Header "token" Token
                              :> Delete '[JSON] VenueFollower

type VenuesIdEventsGet = Prefix :> Capture "venueID" IDType
                         :> "events"
                         :> Header "token" Token
                         :> Get '[JSON] [Event]

type VenuesNearbyGet = Prefix :> "nearby"
                     :> QueryParam "lat" Double
                     :> QueryParam "lon" Double
                     :> QueryParam "distance" Double
                     :> Header "token" Token
                     :> Get '[JSON] [Venue]
