{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Howl.App.Search
  (
    searchHandlers
  ) where

import           Control.Exception.Lifted
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Logger         (MonadLogger, logDebug, logError,
                                               logInfo, runStderrLoggingT)
import           Control.Monad.Trans.Resource
import           Prelude                      as P

import           Data.String.Conversions

import           Database.Esqueleto           (from, ilike, just, select, val,
                                               where_, (%), (++.), (?.), (^.))
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
import           Data.Monoid                  ((<>))
import           Data.Text                    hiding (foldl, map)
import           Data.Text.Lazy               (fromStrict)
import           Data.Text.Lazy.Encoding      (encodeUtf8)
import qualified Data.Time                    as TI
import           Data.Time.Clock

import           Howl.Api.Search
import           Howl.App.Common
import           Howl.Models
import           Howl.Monad
import           Howl.Types
import           Howl.Utils

searchHandlers :: ServerT SearchAPI (HandlerT IO)
searchHandlers =
  getSearchUsers
  :<|> getSearchEvents
  :<|> getSearchVenues

getSearchUsers :: Maybe Text -> Maybe Token -> HandlerT IO [User]
getSearchUsers mq _ =
  case mq of
    Nothing -> throwError err400
    Just q -> runQuery $ do
      res <- select
        $ from
        $ \user -> do
        where_ (user^.UserFirstName  `ilike`  (%) ++. (just $ val q) ++. (%))
        return user
      return $ map entityVal res

getSearchEvents :: Maybe Text -> Maybe Token -> HandlerT IO [Event]
getSearchEvents mq _ =
  case mq of
    Nothing -> throwError err400
    Just q -> runQuery $ do
      res <- select
        $ from
        $ \event -> do
        where_ (event^.EventName  `ilike` (%) ++. val q ++. (%))
        return event
      return $ map entityVal res

getSearchVenues :: Maybe Text -> Maybe Token -> HandlerT IO [Venue]
getSearchVenues mq _ =
  case mq of
    Nothing -> throwError err400
    Just q -> runQuery $ do
      res <- select
        $ from
        $ \venue -> do
        where_ (venue^.VenueName  `ilike` (%) ++. val q ++. (%))
        return venue
      return $ map entityVal res
