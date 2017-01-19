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
  :<|> EventsPut
  :<|> EventsNearbyGet
  :<|> EventsIdGet
  :<|> EventsIdInviteGet
  :<|> EventsIdInviteIdGet
  :<|> EventsIdInviteIdPost
  :<|> EventsIdInviteIdDelete
  :<|> EventsIdRSVPGet
  :<|> EventsIdRSVPUserIdGet
  :<|> EventsIdRSVPUserIdPut
  :<|> EventsIdRSVPUserIdDelete

type Prefix = "events"

type EventsGet = Prefix :> Header "token" Token
                :> Get '[JSON] [Event]

type EventsPut = Prefix :> ReqBody '[JSON] Event
                :> Header "token" Token
                :> Put '[JSON] Event

type EventsIdGet = Prefix :> Capture "eventID" IDType
                   :> Header "token" Token
                   :> Get '[JSON] Event

type EventsIdInviteGet = Prefix :> Capture "eventID" IDType
                        :> "invites"
                        :> Header "token" Token
                        :> Get '[JSON] [Invite]

type EventsIdInviteIdGet = Prefix :> Capture "eventID" IDType
                        :> "invites"
                        :> Capture "friendID" IDType
                        :> Header "token" Token
                        :> Get '[JSON] Invite

type EventsIdInviteIdPost = Prefix :> Capture "eventID" IDType
                        :> "invites"
                        :> Capture "friendID" IDType
                        :> Header "token" Token
                        :> PostCreated '[JSON] Invite

type EventsIdInviteIdDelete = Prefix :> Capture "eventID" IDType
                          :> "invites"
                          :> Capture "friendID" IDType
                          :> Header "token" Token
                          :> Delete '[JSON] Invite

type EventsIdRSVPGet = Prefix :> Capture "eventID" IDType
                       :> "rsvp"
                       :> Header "token" Token
                       :> Get '[JSON] [EventRSVP]

type EventsIdRSVPUserIdGet = Prefix :> Capture "eventID" IDType
                      :> "rsvp" :> Capture "userID" IDType
                      :> Header "token" Token
                      :> Get '[JSON] EventRSVP

type EventsIdRSVPUserIdPut = Prefix :> Capture "eventID" IDType
                      :> "rsvp" :> Capture "userID" IDType
                      :> ReqBody '[JSON] FB.RSVP
                      :> Header "token" Token
                      :> Put '[JSON] EventRSVP

type EventsIdRSVPUserIdDelete = Prefix :> Capture "eventID" IDType
                                :> "rsvp" :> Capture "userID" IDType
                                :> Header "token" Token
                                :> Delete '[JSON] EventRSVP

type EventsNearbyGet = Prefix :> "nearby"
                     :> QueryParam "lat" Double
                     :> QueryParam "long" Double
                     :> QueryParam "distance" Double
                     :> Header "token" Token
                     :> Get '[JSON] [Event]
