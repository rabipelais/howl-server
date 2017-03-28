{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

module Howl.App.Events
  (
    eventsHandlers
  ) where

import           Control.Exception.Lifted
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Logger         (MonadLogger, logError, logInfo,
                                               runStderrLoggingT)
import           Control.Monad.Trans.Resource
import           Prelude                      as P

import           Data.String.Conversions
import qualified Data.Time                    as TI
import           Data.Time.Clock

import           Database.Esqueleto           (from, on, select, where_, (?.),
                                               (^.))
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

import qualified Data.List                    as L
import           Data.Maybe
import           Data.Text                    hiding (foldl, map, replace)

import           Howl.Api.Events
import           Howl.App.Common
import           Howl.Models
import           Howl.Monad
import           Howl.Types
import           Howl.Utils

eventsHandlers :: ServerT EventsAPI (HandlerT IO)
eventsHandlers =
  eventsGetH
  :<|> eventsPutH
  :<|> eventsNearbyGet
  :<|> eventsIdGet
  :<|> eventsIdInviteGet
  :<|> eventsIdInvitesIdGet
  :<|> eventsIdInvitesIdPost
  :<|> eventsIdInvitesIdDelete
  :<|> eventsIdGoingGet
  :<|> eventsIdInterestedGet
  :<|> eventsIdInvitedGet
  :<|> eventsIdRSVPGet
  :<|> eventsIdRSVPUsersIdGet
  :<|> eventsIdRSVPUsersIdPut
  :<|> eventsIdRSVPUsersIdDelete

eventsGetH :: Maybe Token -> HandlerT IO [Event]
eventsGetH mToken = do
  $logInfo $ "Request all events"
  entities <- runQuery $ (select . from $ pure)
  return $ map entityVal entities


eventsPutH :: Event -> Maybe Token -> HandlerT IO Event
eventsPutH event mToken = do
  $logInfo $ "Request put event: " <> (pack . show) event
  runQuery $ do
    getBy (UniqueEventID (eventFbID event)) >>= \case
      Just (Entity k _) -> replace k event >> return event
      Nothing -> insert event >> return event

eventsNearbyGet :: Maybe Double -> Maybe Double -> Maybe Double -> Maybe Token -> HandlerT IO [Event]
eventsNearbyGet (Just lat) (Just lon) d mToken = do
  $logInfo $ "Request events nearby: lat=" <> (pack . show) lat <> ", lon=" <> (pack . show) lon <> ", dist=" <> (pack . show) d
  ui <- tokenUser mToken
  eventEntities <- runQuery $ do
    checkExistsOrThrowError ui err401
    -- TODO: use actual trig functions in postgres
    rawSql
        "SELECT ?? \
        \FROM event INNER JOIN venue \
        \ON event.venue_id=venue.fb_i_d \
        \WHERE (((venue.lat- ?) * 112000) * ((venue.lat- ?) * 112000) + ((venue.lon - ?) * 112000) * ((venue.lon - ?) * 112000)) < (? * ?) AND venue.lat IS NOT NULL AND venue.lon IS NOT NULL"
        [toPersistValue lat, toPersistValue lat, toPersistValue lon, toPersistValue lon, toPersistValue d', toPersistValue d']
  return $ map entityVal eventEntities
  where
    nearby (Just lat1, Just lon1) (lat2, lon2) = (haversine (lat1, lon1) (lat2, lon2)) <= d'
    nearby _ _ = False
    d' = fromMaybe 1000 d
eventsNearbyGet _ _ _ _ = throwError err400

eventsIdGet :: IDType -> Maybe Token -> HandlerT IO Event
eventsIdGet i mToken = runQuery $ do
  $logInfo $ "Request event by id: " <> (pack . show) i
  checkEventOrThrow i

eventsIdInviteGet :: IDType -> Maybe Token -> HandlerT IO [Invite]
eventsIdInviteGet i mToken = do
  $logInfo $ "Request invite to event with id: " <> (pack . show) i
  runQuery $ do
    checkEventOrThrow i
    iEntities <- selectList [InviteEventID ==. i] []
    return $ map entityVal iEntities

eventsIdInvitesIdGet :: IDType -> IDType -> Maybe Token -> HandlerT IO Invite
eventsIdInvitesIdGet ei fi mToken = do
  $logInfo $ "Request the invite from the user to the event " <> (pack . show) ei <> " to user " <> (pack . show) fi
  ui <- tokenUser mToken
  $logInfo $ "-- form user: " <> (pack . show) ui
  runQuery $ do
    checkUsersAndEvent fi ei ui
    getBy (UniqueInvite ui fi ei) >>= \case
      Nothing -> throwError err404
      Just invite -> return $ entityVal invite

eventsIdInvitesIdPost :: IDType -> IDType -> Maybe Token -> HandlerT IO Invite
eventsIdInvitesIdPost ei fi mToken = do
  $logInfo $ "Request post invite to the event " <> (pack . show) ei <> " to user " <> (pack . show) fi
  ui <- tokenUser mToken
  $logInfo $ "-- from user" <> (pack . show) ui
  runQuery $ do
    checkUsersAndEvent fi ei ui
    getBy (UniqueEventRSVP fi ei) >>= \case
      Just (Entity _ (EventRSVP _ _ Fb.NotReplied)) -> post ui
      Just _ -> throwError err409
      Nothing -> post ui
  where
    post ui =
      getBy (UniqueInvite ui fi ei) >>= \case
        Nothing -> do
          inviteEntity <- insertEntity (Invite ui fi ei)
          insertBy (EventRSVP fi ei Fb.NotReplied)
          return $ entityVal inviteEntity
        Just _ -> throwError err409

eventsIdInvitesIdDelete :: IDType -> IDType -> Maybe Token -> HandlerT IO Invite
eventsIdInvitesIdDelete ei fi mToken =  do
  $logInfo $ "Request delete the invite to the event " <> (pack . show) ei <> " to user " <> (pack . show) fi
  ui <- tokenUser mToken
  $logInfo $ "-- from user: " <> (pack . show) ui
  runQuery $ do
    checkUsersAndEvent fi ei ui
    let uniqueInvite = UniqueInvite ui fi ei
    getBy uniqueInvite >>= \case
      Just entity -> deleteBy uniqueInvite >> return (Invite ui fi ei)
      Nothing -> throwError err404

getGuestList ei mToken list = do
  $logInfo $ "Request " <> list <> " to the event: " <> (pack . show) ei
  ui <- tokenUser mToken
  creds' <- asks creds
  manager' <- asks manager
  token <- case mToken of
    Nothing -> throwError err402
    Just t -> return t
  now <- liftIO TI.getCurrentTime
  let userAT = Fb.UserAccessToken ui token now
  let url = "/v2.8/" <> (Fb.idCode ei) <> "/" <> list
  users <-  liftIO $ runResourceT $ do
    usersPager <- Fb.runFacebookT creds' manager' $ Fb.getObject url [("fields", "id,name,email,first_name,last_name,picture{url}")] (Just userAT)
    map fromFbUser <$> go usersPager [] creds' manager'
  (howlUsers, others) <- runQuery $ do
    partitionHowl users
  return (howlUsers ++ others)
  where go pager res creds manager =
          if P.length res < limit
          then do
            (Fb.runFacebookT creds manager $ Fb.fetchNextPage pager) >>= \case
              Just nextPager -> go nextPager (res ++ (Fb.pagerData pager)) creds manager
              Nothing -> return res
          else return res
        limit = 500
        --partitionHowl :: [User] -> ([User], [User])
        partitionHowl fs = partitionM pred fs
        pred u = do
          mUser <- getBy $ UniqueUserID (userFbID u)
          case mUser of
            Nothing -> return False
            Just _ -> return True
        partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
        partitionM p []     = return ([], [])
        partitionM p (x:xs) = do b  <- p x
                                 (ts, fs) <- partitionM p xs
                                 return (if b then (x:ts, fs) else (ts, x:fs))

eventsIdGoingGet :: IDType -> Maybe Token -> HandlerT IO [User]
eventsIdGoingGet ei mToken = getGuestList ei mToken "attending"

eventsIdInterestedGet :: IDType -> Maybe Token -> HandlerT IO [User]
eventsIdInterestedGet ei mToken = getGuestList ei mToken "interested"

eventsIdInvitedGet :: IDType -> Maybe Token -> HandlerT IO [User]
eventsIdInvitedGet ei mToken = getGuestList ei mToken "noreply"

eventsIdRSVPGet :: IDType -> Maybe Token -> HandlerT IO [EventRSVP]
eventsIdRSVPGet ei mToken = do
  $logInfo $ "Request rsvps to the event " <> (pack . show) ei
  ui <- tokenUser mToken
  runQuery $ do
    checkEventOrThrow ei
    checkExistsOrThrowError ui err401
    rEntities <- selectList [EventRSVPEventID ==. ei] []
    return $ map entityVal rEntities

eventsIdRSVPUsersIdGet :: IDType -> IDType -> Maybe Token -> HandlerT IO EventRSVP
eventsIdRSVPUsersIdGet ei fi mToken = do
  $logInfo $ "Request rsvp to the event " <> (pack . show) ei <> " from user " <> (pack . show) fi
  ui <- tokenUser mToken
  runQuery $ do
    checkUsersAndEvent fi ei ui
    getBy (UniqueEventRSVP fi ei) >>= \case
      Nothing -> throwError err404
      Just e -> return $ entityVal e

eventsIdRSVPUsersIdPut :: IDType -> IDType -> Fb.RSVP -> Maybe Token -> HandlerT IO EventRSVP
eventsIdRSVPUsersIdPut ei fi r mToken = do
  $logInfo $ "Request put rsvp to the event " <> (pack . show) ei <> " from user " <> (pack . show) fi <> ": " <> (pack . show) r
  ui <- tokenUser mToken
  runQuery $ do
    checkUsersAndEvent fi ei ui
    let rsvp = EventRSVP fi ei r
    getBy (UniqueEventRSVP fi ei) >>= \case
      Just (Entity k _) -> replace k rsvp
      Nothing -> insert_ rsvp
    return rsvp

eventsIdRSVPUsersIdDelete :: IDType -> IDType -> Maybe Token -> HandlerT IO EventRSVP
eventsIdRSVPUsersIdDelete ei fi mToken = do
  $logInfo $ "Request delete rsvp to the event " <> (pack . show) ei <> " from user " <> (pack . show) fi
  ui <- tokenUser mToken
  runQuery $ do
    checkUsersAndEvent fi ei ui
    let uniqueRSVP = UniqueEventRSVP fi ei
    getBy uniqueRSVP >>= \case
      Just entity -> deleteBy uniqueRSVP >> return (entityVal entity)
      Nothing -> throwError err404


checkUsersAndEvent fi ei ui = do
  checkEventOrThrow ei
  checkExistsOrThrow fi
  checkExistsOrThrowError ui err401
