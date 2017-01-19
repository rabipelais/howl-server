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
  :<|> eventsIdRSVPGet
  :<|> eventsIdRSVPUsersIdGet
  :<|> eventsIdRSVPUsersIdPut
  :<|> eventsIdRSVPUsersIdDelete

eventsGetH :: Maybe Token -> HandlerT IO [Event]
eventsGetH mToken = do
  entities <- runQuery $ (select . from $ pure)
  return $ map entityVal entities


eventsPutH :: Event -> Maybe Token -> HandlerT IO Event
eventsPutH event mToken = runQuery $ do
  getBy (UniqueEventID (eventFbID event)) >>= \case
    Just (Entity k _) -> replace k event >> return event
    Nothing -> insert event >> return event

eventsNearbyGet = undefined

eventsIdGet :: IDType -> Maybe Token -> HandlerT IO Event
eventsIdGet i mToken = runQuery $ do
  checkEventOrThrow i

eventsIdInviteGet :: IDType -> Maybe Token -> HandlerT IO [Invite]
eventsIdInviteGet i mToken = runQuery $ do
  checkEventOrThrow i
  iEntities <- selectList [InviteEventID ==. i] []
  return $ map entityVal iEntities

eventsIdInvitesIdGet :: IDType -> IDType -> Maybe Token -> HandlerT IO Invite
eventsIdInvitesIdGet ei fi mToken = do
  ui <- tokenUser mToken
  runQuery $ do
    checkUsersAndEvent fi ei ui
    getBy (UniqueInvite ui fi ei) >>= \case
      Nothing -> throwError err404
      Just invite -> return $ entityVal invite

eventsIdInvitesIdPost :: IDType -> IDType -> Maybe Token -> HandlerT IO Invite
eventsIdInvitesIdPost ei fi mToken = do
  ui <- tokenUser mToken
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
  ui <- tokenUser mToken
  runQuery $ do
    checkUsersAndEvent fi ei ui
    let uniqueInvite = UniqueInvite ui fi ei
    getBy uniqueInvite >>= \case
      Just entity -> deleteBy uniqueInvite >> return (Invite ui fi ei)
      Nothing -> throwError err404

eventsIdRSVPGet :: IDType -> Maybe Token -> HandlerT IO [EventRSVP]
eventsIdRSVPGet ei mToken = do
  ui <- tokenUser mToken
  runQuery $ do
    checkEventOrThrow ei
    checkExistsOrThrowError ui err401
    rEntities <- selectList [EventRSVPEventID ==. ei] []
    return $ map entityVal rEntities

eventsIdRSVPUsersIdGet :: IDType -> IDType -> Maybe Token -> HandlerT IO EventRSVP
eventsIdRSVPUsersIdGet ei fi mToken = do
  ui <- tokenUser mToken
  runQuery $ do
    checkUsersAndEvent fi ei ui
    getBy (UniqueEventRSVP fi ei) >>= \case
      Nothing -> throwError err404
      Just e -> return $ entityVal e

eventsIdRSVPUsersIdPut :: IDType -> IDType -> Fb.RSVP -> Maybe Token -> HandlerT IO EventRSVP
eventsIdRSVPUsersIdPut ei fi r mToken = do
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
