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

type IDType = Fb.Id

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  fbID IDType
  username Text
  firstName Text
  lastName Text Maybe
  email Text Maybe
  profilePicPath FilePath Maybe
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
