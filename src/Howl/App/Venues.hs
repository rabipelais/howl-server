{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

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

import           Data.Text                    hiding (foldl, map, replace)

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
--  :<|> getVenuesNearbyH
  :<|> getVenuesIdH
  :<|> getVenuesIdFollowersH
  :<|> getVenuesIdFollowersIdH
  :<|> putVenuesIdFollowersIdH
  :<|> deleteVenuesIdFollowersIdH
  :<|> getVenuesIdEventsH

getVenuesH :: Maybe Token -> HandlerT IO [Venue]
getVenuesH mToken = do
  entities <- runQuery $ (select . from $ pure)
  return $ map entityVal entities

putVenuesH :: Venue -> Maybe Token -> HandlerT IO Venue
putVenuesH venue mToken = runQuery $ do
  getBy (UniqueVenueID (venueFbID venue)) >>= \case
    Just (Entity k _) -> replace k venue >> return venue
    Nothing -> insert venue >> return venue

getVenuesIdH :: IDType -> Maybe Token -> HandlerT IO Venue
getVenuesIdH i mToken = runQuery $ do
  checkVenueOrThrow i

getVenuesIdFollowersH :: IDType -> Maybe Token -> HandlerT IO [User]
getVenuesIdFollowersH = undefined

getVenuesIdFollowersIdH :: IDType -> IDType -> Maybe Token -> HandlerT IO VenueFollower
getVenuesIdFollowersIdH = undefined

putVenuesIdFollowersIdH :: IDType -> IDType -> Maybe Token -> HandlerT IO VenueFollower
putVenuesIdFollowersIdH = undefined

deleteVenuesIdFollowersIdH :: IDType -> IDType -> Maybe Token -> HandlerT IO VenueFollower
deleteVenuesIdFollowersIdH = undefined

getVenuesIdEventsH :: IDType -> Maybe Token -> HandlerT IO [Event]
getVenuesIdEventsH = undefined
