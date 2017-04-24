{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Howl.App.Common where

import Prelude as P
import           Control.Exception.Lifted
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Logger         (MonadLogger, logError, logInfo,
                                               runStderrLoggingT)
import           Control.Monad.Trans.Resource
import qualified Data.HashMap.Strict as HM

import           Data.String.Conversions
import           Data.List.Split as L
import Data.List as L
import qualified Data.Time                    as TI
import Data.Time.Clock
import Data.Time.Calendar
import Data.Foldable
import Data.Traversable
import Data.Aeson as A
import Data.Aeson (FromJSON, parseJSON, withObject, (.:), (.:?), Value(..))
import Data.Aeson.Types (parseMaybe, Parser, Object)

import qualified Data.ByteString.Char8 as B
import           Database.Esqueleto           (from, select, (^.))
import qualified Database.Esqueleto           as E
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite      as Sql

import Data.Conduit as CL
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CL

import           Network.HTTP.Conduit         (Manager, newManager,
                                               tlsManagerSettings)
import           Network.Wai
import           Network.Wai.Handler.Warp     as Warp


import           Servant

import           Data.Text      as T          hiding (foldl, map)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Maybe

import           Howl.Api.Common as Api
import qualified Howl.Facebook                as Fb
import           Howl.Models as Model
import           Howl.Monad
import           Howl.Types
import           Howl.Utils

import Debug.Trace

runDb :: (MonadLogger (HandlerT IO), MonadError e (HandlerT IO))
      => ConnectionPool -> e -> SqlPersistT (HandlerT IO) a -> HandlerT IO a
runDb pool err q =
  catch (runSqlPool q pool) $ \(SomeException e) -> do
    $logError "runSqlPool failed."
    $logError $ "Error: " <> (pack . show) e
    throwError err

runQuery :: SqlPersistT (HandlerT IO) a -> HandlerT IO a
runQuery query = do
  pool <- asks db
  runDb pool err500 query

checkExistsOrThrow i = checkExistsOrThrowError i err404

checkExistsOrThrowError i e = do
  mUser <- getBy $ UniqueUserID i
  case mUser of
    Nothing -> throwError e
    Just (Entity k u) -> return u

checkEventOrThrow i = do
  mEvent <- getBy $ UniqueEventID i
  case mEvent of
    Nothing -> throwError err404
    Just (Entity k u) -> return u

checkVenueOrThrow i = do
  mVenue <- getBy $ UniqueVenueID i
  case mVenue of
    Nothing -> throwError err404
    Just (Entity k u) -> return u

fromFbUser :: Fb.User -> User
fromFbUser u = user
  where
    fbID = Fb.userId u
    fullName = fromMaybe (Fb.idCode fbID) (Fb.userName u)
    username = fromMaybe (Fb.idCode fbID) (Fb.userUsername u)
    firstName = Fb.userFirstName u
    lastName = Fb.userLastName u
    email = Fb.userEmail u
    profilePicPath = unpack <$> Fb.userPicSource u
    user = User fbID fullName username firstName lastName email profilePicPath True

fromFbEvent :: Fb.Event -> Model.Event
fromFbEvent e = event
  where
    fbID = Fb.eventId e
    description = fromMaybe "" (Fb.eventDescription e)
    name = fromMaybe "Unnamed event" (Fb.eventName e)
    attending = Fb.eventAttending e
    maybeCount = Fb.eventMaybe e
    declined = Fb.eventDeclined e
    startTime = fromMaybe (UTCTime (fromGregorian 2017 01 01) (secondsToDiffTime 0)) (Fb.eventStartTime e)
    endTime = fromMaybe (UTCTime (fromGregorian 2017 01 01) (secondsToDiffTime 0)) (Fb.eventEndTime e)
    venueId = fromMaybe "" (Fb.placeId =<< Fb.eventPlace e)
    coverPicPath = unpack <$> Fb.eventCoverSource e
    event = Model.Event fbID description name attending maybeCount declined startTime endTime venueId coverPicPath

fromFbVenue :: Fb.Place -> Venue
fromFbVenue v = venue
  where
    fbID = fromMaybe "" $ Fb.placeId v
    coverPicPath = unpack <$> Fb.placeCoverSource v
    profilePicPath = unpack <$> Fb.placePicSource v
    category = Fb.placeCategory v
    about = fromMaybe "" (Fb.placeAbout v)
    description = fromMaybe "" (Fb.placeDescription v)
    name = fromMaybe "Unnamed event" (Fb.placeName v)
    city = Fb.placeLocation v >>= Fb.locationCity
    country = Fb.placeLocation v >>= Fb.locationCountry
    street = Fb.placeLocation v >>= Fb.locationStreet
    zip = Fb.placeLocation v >>= Fb.locationZip
    geo = Fb.placeLocation v >>= Fb.locationCoords
    venue = Venue fbID coverPicPath profilePicPath category about description name city country street zip (Fb.latitude <$> geo) (Fb.longitude <$> geo) Nothing


intoApiEvent ui e = do
  let i = eventFbID e
  goingEntities <- E.select
    $ E.from
    $ \(friend `E.InnerJoin` user `E.InnerJoin` eventRsvp) -> do
    E.on (user^.UserFbID E.==. eventRsvp^.EventRSVPUserID)
    E.on (friend^.FollowshipTargetId E.==. user^.UserFbID)
    E.where_ (
      (friend^.FollowshipSourceId E.==. E.val ui)
      E.&&. (friend^.FollowshipStatus E.==. E.val Accepted)
      E.&&.(eventRsvp^.EventRSVPEventID E.==. E.val i)
      E.&&. (eventRsvp^.EventRSVPRsvp E.==. E.val Fb.Attending))
    return user
  interestedEntities <- E.select
    $ E.from
    $ \(friend `E.InnerJoin` user `E.InnerJoin` eventRsvp) -> do
    E.on (user^.UserFbID E.==. eventRsvp^.EventRSVPUserID)
    E.on (friend^.FollowshipTargetId E.==. user^.UserFbID)
    E.where_ (
      (friend^.FollowshipSourceId E.==. E.val ui)
      E.&&. (friend^.FollowshipStatus E.==. E.val Accepted)
      E.&&.(eventRsvp^.EventRSVPEventID E.==. E.val i)
      E.&&. (eventRsvp^.EventRSVPRsvp E.==. E.val Fb.Maybe))
    return user
  let (goingNames, goingPics) = unzip $ map (\u -> (userName u, fromMaybe "" $ userProfilePicPath u)) $ map entityVal goingEntities
  let (interestedNames, interestedPics) = unzip $ map (\u -> (userName u, fromMaybe "" $ userProfilePicPath u)) $ map entityVal interestedEntities
  let t = T.pack $ TI.formatTime TI.defaultTimeLocale "%b %e" (eventStartTime e)
  return (Api.Event e t
          (L.length goingNames) goingNames goingPics
          (L.length interestedNames) interestedNames interestedPics)
