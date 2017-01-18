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
  :<|> eventsIdInvitePost
  :<|> eventsIdInviteDelete
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

eventsIdGet = undefined

eventsIdInviteGet = undefined

eventsIdInvitePost = undefined

eventsIdInviteDelete = undefined

eventsIdRSVPUsersIdGet = undefined

eventsIdRSVPUsersIdPut = undefined

eventsIdRSVPUsersIdDelete = undefined
