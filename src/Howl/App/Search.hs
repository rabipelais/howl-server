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
getSearchUsers _ _ = throwError err405
getSearchEvents :: Maybe Text -> Maybe Token -> HandlerT IO [Event]
getSearchEvents _ _ = throwError err405
getSearchVenues :: Maybe Text -> Maybe Token -> HandlerT IO [Venue]
getSearchVenues _ _ = throwError err405
