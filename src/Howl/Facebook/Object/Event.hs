{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module Howl.Facebook.Object.Event where

import           Control.Applicative
import           Control.Monad                (mzero)
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Data.Aeson                   ((.:), (.:?))
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Char
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import           Data.Time
import           Data.Typeable                (Typeable)
import           GHC.Generics

import qualified Control.Monad.Trans.Resource as R
import           Data.Aeson                   as A

import           Howl.Facebook.Graph
import           Howl.Facebook.Monad
import           Howl.Facebook.Object.Checkin
import           Howl.Facebook.Pager
import           Howl.Facebook.Types

type EventId = Id

data Event =
  Event { eventId          :: EventId
        , eventName        :: Maybe Text
        , eventCategory    :: Maybe Text
        , eventDescription :: Maybe Text
        , eventStartTime   :: Maybe UTCTime
        , eventEndTime     :: Maybe UTCTime
        , eventPlace       :: Maybe Place
        , eventRSVP        :: Maybe RSVP
        , eventCoverSource :: Maybe Text
        , eventOwner       :: Maybe Owner}
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance A.FromJSON Event where
    parseJSON (A.Object v) =
      Event <$> v .:  "id"
           <*> v .:? "name"
           <*> v .:? "category"
           <*> v .:? "description"
           <*> v .:? "start_time"
           <*> v .:? "end_time"
           <*> v .:? "place"
           <*> v .:? "rsvp_status"
           <*> do cover <- v .:? "cover"
                  case cover of
                     Nothing -> return Nothing
                     Just c -> c .:? "source"
           <*> v .:? "owner"
    parseJSON _ = mzero

data Owner = Owner {ownerId :: Maybe Id, ownerName :: Maybe Text}
  deriving (Ord, Show, Read, Eq, Generic, Typeable)

instance A.ToJSON Owner where
  -- For efficiency, we write a simple toEncoding implementation, as
  -- the default version uses toJSON.
  toEncoding = genericToEncoding defaultOptions

instance A.FromJSON Owner where
  parseJSON (A.Object v) =
    Owner <$> v .:? "id"
          <*> v .:? "name"
  parseJSON _ = mzero

data RSVP = Attending | Created | Declined
          | Maybe | Unsure | NotReplied
          deriving (Ord, Show, Read, Eq, Generic, Typeable)

instance A.ToJSON RSVP where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = snakeCase}

instance A.FromJSON RSVP where
  parseJSON (A.String v) = case v of
    "attending" -> return Attending
    "created" -> return Created
    "declined" -> return Declined
    "maybe" -> return Maybe
    "unsure" -> return Unsure
    "not_replied" -> return NotReplied
    x -> fail $ "Unexpected RSVP value: " ++ show x
  parseJSON invalid = typeMismatch "RSVP" invalid


-- getFromUser userAT@(UserAccessToken uid _ _) target = getObject ("/" <> (idCode . uid) <> "/" <> target)

-- getUserEvents userAT creds manager limit = do
--   fbEvents <- runFacebookT creds manager $ getFromUser userAT "events" $ [("limit", encode limit)] (Just userAT)
