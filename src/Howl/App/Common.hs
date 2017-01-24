{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Howl.App.Common where

import           Control.Exception.Lifted
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Logger         (MonadLogger, logError, logInfo,
                                               runStderrLoggingT)
import           Control.Monad.Trans.Resource

import           Data.String.Conversions

import qualified Data.ByteString.Char8 as B
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

import           Data.Text                    hiding (foldl, map)

import           Howl.Models
import           Howl.Monad
import           Howl.Types
import           Howl.Utils

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

getFbVenuesIdNearby :: (MonadResource m, MonadBaseControl IO m) => Fb.UserAccessToken -> Double -> Double -> Double ->  Fb.FacebookT anyAuth m (Fb.Pager IDType)
getFbVenuesIdNearby userAT lat lon distance = do
  let params = [ ("fields", "id")
               , ("type", "place")
               , ("center", (B.pack . show) lat <> "," <> (B.pack . show) lon)
               , ("limit", "1000")
               , ("distance", (B.pack . show) distance)]
  Fb.searchObjects "place" "" params (Just userAT)
