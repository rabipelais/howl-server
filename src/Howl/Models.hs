{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Howl.Models where

import           Data.Aeson
import           Data.Text

import           Control.Applicative
import           Data.Time.Clock
import           GHC.Generics
import           System.Directory

import           Database.Persist.TH
import qualified Howl.Facebook              as Fb
import           Howl.Facebook.Object.Event (RSVP)
import           Howl.Facebook.Persistent   ()

import           Howl.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  fbID IDType
  name Text
  username Text
  firstName Text Maybe
  lastName Text Maybe
  email Text Maybe
  profilePicPath FilePath Maybe
  private Bool default=true
  UniqueUserID fbID
  UniqueUsername username
  deriving Eq Read Show Generic

Followship json
  sourceId IDType
  targetId IDType
  status FollowStatus
  UniqueFollowshipID sourceId targetId
  deriving Eq Read Show

Event json
  fbID IDType
  description Text
  name Text
  attendingCount Int default=0
  maybeCount Int default=0
  declinedCount Int default=0
  startTime UTCTime
  endTime UTCTime
  venueId IDType
  coverPicPath FilePath Maybe
  UniqueEventID fbID
  deriving Eq Read Show

EventRSVP json
  userID IDType
  eventID IDType
  rsvp RSVP
  UniqueEventRSVP userID eventID
  deriving Eq Read Show

Invite json
  sourceID IDType
  targetID IDType
  eventID IDType
  UniqueInvite sourceID targetID eventID
  deriving Eq Read Show

Venue json
  fbID IDType
  coverPicPath FilePath Maybe
  profilePicPath FilePath Maybe
  category Text Maybe
  about Text
  description Text
  name Text
  city Text Maybe
  country Text Maybe
  street Text Maybe
  zip Text Maybe
  lat Double Maybe
  lon Double Maybe
  rating Double Maybe
  UniqueVenueID fbID
  deriving Eq Read Show

VenueFollower json
  venueID IDType
  userID IDType
  UniqueVenueFollower venueID userID
  deriving Eq Read Show

Connect json
  userID IDType
  eventID IDType
  UniqueConnect userID eventID
  deriving Eq Read Show

ConnectFriends json
  connect ConnectId
  friendID IDType
  UniqueConnectFriends connect friendID
  deriving Eq Read Show

Device json
  type DeviceType
  userId IDType
  deviceId Text
  UniqueUserDevice type userId deviceId
  deriving Eq Read Show
|]
