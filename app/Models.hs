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

module Models where

import Data.Aeson
import Data.Text

import Control.Applicative
import Data.Time.Clock

import Database.Persist.TH

import Types

type IDType = Text

-- UserEvent models many-to-many relationship
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  fbID IDType
  firstName Text
  lastName Text
  email String
  UniqueUserID fbID
  deriving Eq Read Show

Event json
  fbID IDType
  description Text
  name Text
  startTime UTCTime
  endTime UTCTime
  rsvp RSVP
  UniqueEventID fbID
  deriving Eq Read Show

UserEvent json
  userFbID IDType
  eventFbID IDType
  UniqueUserEvent userFbID eventFbID
|]
