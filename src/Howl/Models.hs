{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Howl.Models where

import Data.Aeson
import Data.Text

import Control.Applicative
import Data.Time.Clock
import System.Directory
import GHC.Generics

import Database.Persist.TH
import qualified Howl.Facebook as Fb
import Howl.Facebook.Persistent ()
import Howl.Facebook.Object.Event (RSVP)

import Howl.Types

type IDType = Fb.Id

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  fbID IDType
  username Text
  firstName Text
  lastName Text Maybe
  email Text Maybe
  UniqueUserID fbID
  UniqueUsername username
  deriving Eq Read Show Generic

UserProfilePics
  userId UserId
  profilePicPath FilePath
  UniqueUserProfilePic userId profilePicPath
  deriving Eq Read Show

Friendship json
  userSourceId UserId
  userTargetId UserId
  status FriendshipStatus
  UniqueFriendshipID userSourceId userTargetId
  deriving Eq Read Show

Event json
  fbID IDType
  description Text
  name Text
  startTime UTCTime
  endTime UTCTime
  venueId IDType
  UniqueEventID fbID
  deriving Eq Read Show

EventCoverPic
  eventId EventId
  coverPicPath FilePath
  UniqueEventCoverPic eventId coverPicPath
  deriving Eq Read Show

UserEvent json
  userId UserId
  eventId EventId
  rsvp RSVP
  UniqueUserEvent userId eventId
  deriving Eq Read Show

Venue json
  fbID IDType
  coverPicPath FilePath
  description Text
  name Text
  location Text
  rating Int Maybe
  UniqueVenueID fbID
  deriving Eq Read Show

VenueFollowers json
  venueId VenueId
  userId UserId
  UniqueVenueFollowers venueId userId
  deriving Eq Read Show
|]
