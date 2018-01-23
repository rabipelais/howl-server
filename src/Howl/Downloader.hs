{-# LANGUAGE FlexibleContexts  #-}

module Howl.Downloader where

import           Control.Exception.Lifted
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Logger         (MonadLogger, logError, logInfo,
                                               runStderrLoggingT)
import           Control.Monad.Trans.Resource

import           Data.Foldable
import           Data.Traversable
import           Data.Maybe

import           Data.Time.Clock
import           Data.Time.Calendar
import Data.Time.Clock.POSIX
import           Data.Conduit                 as CL
import qualified Data.Conduit.Combinators     as CL
import qualified Data.Conduit.List            as CL
import           Data.String.Conversions
import           Data.List.Split              as L
import           Data.List                    as L
import qualified Data.HashMap.Strict          as HM
import           Data.Aeson                   as A
import           Data.Aeson (FromJSON, parseJSON, withObject, (.:), (.:?), Value(..))
import           Data.Aeson.Types (parseMaybe, Parser, Object)

import qualified Data.ByteString.Char8        as B
import           Data.Text                    hiding (foldl, map)
import           Data.Text.Encoding           (encodeUtf8)

import qualified Howl.Facebook                as Fb

import           Howl.Models
import           Howl.Monad
import           Howl.Types
import           Howl.Utils
import           Howl.App.Common


getFbVenuesIdNearby :: (MonadResource m, MonadBaseControl IO m) => Fb.UserAccessToken -> Double -> Double -> Double -> Double -> Fb.FacebookT anyAuth m (Fb.Pager IDType)
getFbVenuesIdNearby userAT lat lon distance limit = do
  let params = [ ("fields", "id")
               , ("type", "place")
               , ("center", (B.pack . show) lat <> "," <> (B.pack . show) lon)
               , ("limit", (B.pack . show) limit)
               , ("distance", (B.pack . show) distance)]
  Fb.searchObjects "place" "" params (Just userAT)

getEventsFromVenuesNearby :: (MonadResource m, MonadBaseControl IO m) => Fb.UserAccessToken -> Fb.Pager IDType -> Maybe Int -> Fb.FacebookT anyAuth m [(Venue, Maybe (Fb.Pager Fb.Event))]
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
              "category",
              "cover.fields(id,source)",
              "picture.type(normal)",
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

-- USAGE

    -- fromNearby creds' manager' token = do
    --   now <- liftIO TI.getCurrentTime
    --   posixTime <- liftIO getPOSIXTime
    --   let userAT = Fb.UserAccessToken ui token now
    --   ves <- liftIO $ runResourceT $ Fb.runFacebookT creds' manager' (getVenuesAndEventsNearby userAT lat lon distance 1000 (Just (truncate posixTime)))
    --   mapM_ (\(v, _) -> insertBy v) ves
    --   mapM_ (\(_, es) -> mapM_ (insertBy . fromFbEvent) es) ves
    --   return $ P.concat $ map (\(_, es) -> map fromFbEvent es) ves

--data FbSource = User
--              | VenuesNearby userAT lat lon distance limit
--              |

--downloaderWorker chan queue = consumeMsgs ch queue Ack worker
--  where
--    worker (msg, metadata) = do
--      let body = decode (msgBody msg)
--      case body of
