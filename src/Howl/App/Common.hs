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

import qualified Howl.Facebook                as Fb

import           Servant

import           Data.Text                    hiding (foldl, map)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Maybe

import           Howl.Models
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
    username = fromMaybe (Fb.idCode fbID) (Fb.userUsername u)
    firstName = fromMaybe username (Fb.userFirstName u)
    lastName = Fb.userLastName u
    email = Fb.userEmail u
    profilePicPath = Nothing
    user = User fbID username firstName lastName email profilePicPath

fromFbEvent :: Fb.Event -> Event
fromFbEvent e = event
  where
    fbID = Fb.eventId e
    description = fromMaybe "" (Fb.eventDescription e)
    name = fromMaybe "Unnamed event" (Fb.eventName e)
    startTime = fromMaybe (UTCTime (fromGregorian 2017 01 01) (secondsToDiffTime 0)) (Fb.eventStartTime e)
    endTime = fromMaybe (UTCTime (fromGregorian 2017 01 01) (secondsToDiffTime 0)) (Fb.eventEndTime e)
    venueId = fromMaybe "" (Fb.placeId =<< Fb.eventPlace e)
    coverPicPath = unpack <$> Fb.eventCoverSource e
    event = Event fbID description name startTime endTime venueId coverPicPath

fromFbVenue :: Fb.Place -> Venue
fromFbVenue v = venue
  where
    fbID = fromMaybe "" $ Fb.placeId v
    coverPicPath = unpack <$> Fb.placeCoverSource v
    about = fromMaybe "" (Fb.placeAbout v)
    description = fromMaybe "" (Fb.placeDescription v)
    name = fromMaybe "Unnamed event" (Fb.placeName v)
    city = Fb.placeLocation v >>= Fb.locationCity
    country = Fb.placeLocation v >>= Fb.locationCountry
    street = Fb.placeLocation v >>= Fb.locationStreet
    zip = Fb.placeLocation v >>= Fb.locationZip
    geo = Fb.placeLocation v >>= Fb.locationCoords
    venue = Venue fbID coverPicPath about description name city country street zip (Fb.latitude <$> geo) (Fb.longitude <$> geo) Nothing

getFbVenuesIdNearby :: (MonadResource m, MonadBaseControl IO m) => Fb.UserAccessToken -> Double -> Double -> Double -> Double -> Fb.FacebookT anyAuth m (Fb.Pager IDType)
getFbVenuesIdNearby userAT lat lon distance limit = do
  let params = [ ("fields", "id")
               , ("type", "place")
               , ("center", (B.pack . show) lat <> "," <> (B.pack . show) lon)
               , ("limit", (B.pack . show) limit)
               , ("distance", (B.pack . show) distance)]
  Fb.searchObjects "place" "" params (Just userAT)

getEventsFromVenuesNearby :: (MonadResource m, MonadBaseControl IO m) => Fb.UserAccessToken -> Fb.Pager IDType -> Maybe Day -> Fb.FacebookT anyAuth m [(Venue, Maybe (Fb.Pager Fb.Event))]
getEventsFromVenuesNearby userAT vPager mSince = do
  (L.concat . catMaybes . map (parseMaybe parseEach)) <$> sequence requests
  where
    since = case mSince of
        Nothing -> ""
        Just s -> ".since(" <> Fb.encodeFbParam s <> ")"
    venueChunks = L.chunksOf 50 (Fb.pagerData vPager)
    eventsFields = ["id",
                     "type",
                     "name",
                     "cover.fields(id,source)",
                     "picture.type(large)",
                     "description",
                     "start_time",
                     "end_time",
                     "category",
                     "place",
                     "rsvp_status",
                     "attending_count",
                     "declined_count",
                     "maybe_count",
                     "owner",
                     "noreply_count"]

    fields :: [B.ByteString]
    fields = ["id",
              "name",
              "about",
              "emails",
              "cover.fields(id,source)",
              "picture.type(large)",
              "location",
              "events.fields(" <> (fold $ L.intersperse (B.pack ",") eventsFields) <> ")"]
    args = [ ("fields", (fold $ L.intersperse (B.pack ",") fields) <> since)
           ]
    requests =
      map (\ids -> Fb.getObject "/v2.8/" (("ids", (fold $ L.intersperse (B.pack ",") (map (encodeUtf8 . Fb.idCode) ids))) : args) (Just userAT)) venueChunks
    parseEach :: Value -> Parser [(Venue, Maybe (Fb.Pager Fb.Event))]
    parseEach = withObject "venue/events" $ \o ->
      for (HM.toList o) $ \(_, ve) -> do
        (venue :: Fb.Place) <- parseJSON ve
        obj <- parseJSON ve
        (eventsPager :: Maybe (Fb.Pager Fb.Event)) <- obj.:?"events"
        return $ (fromFbVenue venue, eventsPager)

getVenuesAndEventsNearby userAT lat lon distance limit mSince = do
  venuePager <- getFbVenuesIdNearby userAT lat lon distance limit
  ves <- getEventsFromVenuesNearby userAT venuePager mSince
  ves' <- for ves $ \(v, mp) -> do
    let es = case mp of
          Nothing -> return $ CL.sourceList []
          Just p -> Fb.fetchAllNextPages p
    es' <- es
    es'' <- CL.runConduit $ es' CL.=$ CL.sinkList
    return $ (v, es'')
  return ves'
