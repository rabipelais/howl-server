{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Howl.Api.Events where

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

import           Howl.Models
import           Howl.Types

import           Servant.API


type EventsAPI =
  EventsGet
  :<|> EventsPost
  :<|> EventsNearbyGet
  :<|> EventsIdGet
  :<|> EventsIdInviteGet
  :<|> EventsIdInvitePost
  :<|> EventsIdInviteDelete
  :<|> EventsIdRSVPUserIdGet
  :<|> EventsIdRSVPUserIdPut
  :<|> EventsIdRSVPUserIdDelete

type Prefix = "events"

type EventsGet = Prefix :> Header "token" Token
                :> Get '[JSON] [Event]

type EventsPost = Prefix :> ReqBody '[JSON] Event
                :> Header "token" Token
                :> PostCreated '[JSON] Event

type EventsIdGet = Prefix :> Capture "eventID" IDType
                   :> Header "token" Token
                   :> Get '[JSON] Event

type EventsIdInviteGet = Prefix :> Capture "eventID" IDType
                        :> "invite"
                        :> Capture "friendID" IDType
                        :> Header "token" Token
                        :> Get '[JSON] Bool

type EventsIdInvitePost = Prefix :> Capture "eventID" IDType
                        :> "invite"
                        :> Capture "friendID" IDType
                        :> Header "token" Token
                        :> PostCreated '[JSON] IDType

type EventsIdInviteDelete = Prefix :> Capture "eventID" IDType
                          :> "invite"
                          :> Capture "friendID" IDType
                          :> Header "token" Token
                          :> Delete '[JSON] IDType


type EventsIdRSVPUserIdGet = Prefix :> Capture "eventID" IDType
                      :> "rsvp" :> Capture "userID" IDType
                      :> ReqBody '[JSON] FB.RSVP
                      :> Header "token" Token
                      :> Get '[JSON] FB.RSVP

type EventsIdRSVPUserIdPut = Prefix :> Capture "eventID" IDType
                      :> "rsvp" :> Capture "userID" IDType
                      :> ReqBody '[JSON] FB.RSVP
                      :> Header "token" Token
                      :> Put '[JSON] FB.RSVP

type EventsIdRSVPUserIdDelete = Prefix :> Capture "eventID" IDType
                                :> "rsvp" :> Capture "userID" IDType
                                :> Header "token" Token
                                :> Delete '[JSON] FB.RSVP

type EventsNearbyGet = Prefix :> "nearby"
                     :> QueryParam "lat" Double
                     :> QueryParam "long" Double
                     :> QueryParam "distance" Double
                     :> Header "token" Token
                     :> Get '[JSON] [Event]
