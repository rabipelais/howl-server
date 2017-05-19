{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

module Howl.App.Venues
  (
    venuesHandlers
  ) where

import           Control.Exception.Lifted
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Logger         (MonadLogger, logError, logInfo,
                                               runStderrLoggingT)
import           Control.Monad.Trans.Resource

import           Data.String.Conversions

import           Database.Esqueleto           (from, select, (^.))
import qualified Database.Esqueleto           as E
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite      as Sql

import           Network.HTTP.Conduit         (Manager, newManager,
                                               tlsManagerSettings)
import           Network.Wai
import           Network.Wai.Handler.Warp     as Warp

import qualified Howl.Facebook                as Fb

import           Servant

import           Data.Maybe
import           Data.Text                    hiding (foldl, map, replace)

import           Howl.Api.Common              as Api
import           Howl.Api.Venues
import           Howl.App.Common
import           Howl.Models
import           Howl.Monad
import           Howl.Types
import           Howl.Utils

venuesHandlers :: ServerT VenuesAPI (HandlerT IO)
venuesHandlers =
  getVenuesH
  :<|> putVenuesH
  :<|> getVenuesNearbyH
  :<|> getVenuesIdH
  :<|> getVenuesIdFollowersH
  :<|> getVenuesIdFollowersIdH
  :<|> putVenuesIdFollowersIdH
  :<|> deleteVenuesIdFollowersIdH
  :<|> getVenuesIdEventsH

getVenuesH :: Maybe Token -> HandlerT IO [Venue]
getVenuesH mToken = do
  $logInfo $ "Request all venues"
  entities <- runQuery $ (select . from $ pure)
  return $ map entityVal entities

putVenuesH :: Venue -> Maybe Token -> HandlerT IO Venue
putVenuesH venue mToken = do
  $logInfo $ "Request put venue " <> (pack . show) venue
  runQuery $ do
    getBy (UniqueVenueID (venueFbID venue)) >>= \case
      Just (Entity k _) -> replace k venue >> return venue
      Nothing -> insert venue >> return venue

getVenuesIdH :: IDType -> Maybe Token -> HandlerT IO Venue
getVenuesIdH i mToken = do
  $logInfo $ "Request get venue by id: " <> (pack . show) i
  runQuery $ do
    checkVenueOrThrow i

getVenuesIdFollowersH :: IDType -> Maybe Token -> HandlerT IO [User]
getVenuesIdFollowersH i mToken = do
  $logInfo $ "Request get followers of venue with id: " <> (pack . show) i
  runQuery $ do
    checkVenueOrThrow i
    eventEntities <- E.select $ E.distinct
      $ E.from
      $ \(user `E.InnerJoin` follower) -> do
      E.on (user^.UserFbID E.==. follower^.VenueFollowerUserID
            E.&&. follower^.VenueFollowerVenueID E.==. E.val i)
      return user
    return $ map entityVal eventEntities

getVenuesIdFollowersIdH :: IDType -> IDType -> Maybe Token -> HandlerT IO VenueFollower
getVenuesIdFollowersIdH vi fi mToken = do
  $logInfo $ "Request get if user is following venue: " <> (pack . show) vi <> ", user: " <> (pack . show) fi
  ui <- tokenUser mToken
  $logInfo $ "-- by user: " <> (pack . show) ui
  runQuery $ do
    checkVenueOrThrow vi
    checkExistsOrThrow fi
    checkExistsOrThrowError ui err401
    getBy (UniqueVenueFollower vi fi) >>= \case
      Nothing -> throwError err404
      Just e -> return $ entityVal e

putVenuesIdFollowersIdH :: IDType -> IDType -> Maybe Token -> HandlerT IO VenueFollower
putVenuesIdFollowersIdH vi fi mToken = do
  $logInfo $ "Request put user is following venue: " <> (pack . show) vi <> ", user: " <> (pack . show) fi
  ui <- tokenUser mToken
  $logInfo $ "-- by user: " <> (pack . show) ui
  runQuery $ do
    checkVenueOrThrow vi
    checkExistsOrThrow fi
    checkExistsOrThrowError ui err401
    when (fi /= ui) (throwError err403)
    let vf = VenueFollower vi fi
    getBy (UniqueVenueFollower vi fi) >>= \case
      Nothing -> insert_ vf
      Just (Entity k _) -> replace k vf
    return vf

deleteVenuesIdFollowersIdH :: IDType -> IDType -> Maybe Token -> HandlerT IO VenueFollower
deleteVenuesIdFollowersIdH vi fi mToken = do
  $logInfo $ "Request delete if user is following venue: " <> (pack . show) vi <> ", user: " <> (pack . show) fi
  ui <- tokenUser mToken
  $logInfo $ "-- by user: " <> (pack . show) ui
  runQuery $ do
    checkVenueOrThrow vi
    checkExistsOrThrow fi
    checkExistsOrThrowError ui err401
    when (fi /= ui) (throwError err403)
    let uFollow = UniqueVenueFollower vi fi
    getBy uFollow >>= \case
      Just entity -> deleteBy uFollow >> return (entityVal entity)
      Nothing -> throwError err404

getVenuesIdEventsH :: IDType -> Maybe Token -> HandlerT IO [Api.Event]
getVenuesIdEventsH vi mToken = do
  $logInfo $ "Request events in venue: " <> (pack . show) vi
  ui <- tokenUser mToken
  $logInfo $ "-- by user: " <> (pack . show) ui
  now <- liftIO TI.getCurrentTime
  runQuery $ do
    checkVenueOrThrow vi
    checkExistsOrThrowError ui err401
    entities <- select $ E.distinct
      $ from $ \event -> do
      E.where_ (event^.EventVenueId E.==. E.val vi
                 E.&&.event^.EventStartTime E.>=. E.val now)
      E.orderBy [E.asc (event^.EventStartTime)]
      return event
    let es = map entityVal entities
    mapM (intoApiEvent ui) es

getVenuesNearbyH :: Maybe Double -> Maybe Double -> Maybe Double -> Maybe Token -> HandlerT IO [Venue]
getVenuesNearbyH (Just lat) (Just lon) d mToken = do
  $logInfo $ "Request venues nearby: lat=" <> (pack . show) lat <> ", lon=" <> (pack . show) lon <> ", dist=" <> (pack . show) d
  ui <- tokenUser mToken
  $logInfo $ "-- by user: " <> (pack . show) ui
  eventEntities <- runQuery $ do
    checkExistsOrThrowError ui err401
    -- TODO: use actual trig functions in postgres
    rawSql
        "SELECT ?? \
        \FROM venue  \
        \WHERE (((venue.lat- ?) * 112000) * ((venue.lat- ?) * 112000) + ((venue.long - ?) * 112000) * ((venue.long - ?) * 112000)) < (? * ?) AND venue.lat IS NOT NULL AND venue.long IS NOT NULL"
        [toPersistValue lat, toPersistValue lat, toPersistValue lon, toPersistValue lon, toPersistValue d', toPersistValue d']
  return $ map entityVal eventEntities
  where
    nearby (Just lat1, Just lon1) (lat2, lon2) = (haversine (lat1, lon1) (lat2, lon2)) <= d'
    nearby _ _ = False
    d' = fromMaybe 1000 d
getVenuesNearbyH _ _ _ _ = throwError err400
