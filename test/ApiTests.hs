{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module ApiTests where

import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit

import           Howl
import qualified Howl.Facebook                as Fb
import qualified Howl.Logger                  as Logger

import           Network.Wai
import           Network.Wai.Handler.Warp     as Warp
import           Servant.API
import           Servant.Client

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Control.Exception            (ErrorCall (..), throwIO)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Resource
import           Data.Monoid                  ((<>))
import           Data.Text
import           Data.Time.Clock
import           Network.HTTP.Client          (Manager, defaultManagerSettings,
                                               newManager)
import           Network.HTTP.Conduit         (tlsManagerSettings)
import           Test.Hspec                   (Spec, describe, hspec, it)
import           Test.Hspec.Wai               (WaiExpectation, WaiSession,
                                               delete, get, matchBody, request,
                                               shouldRespondWith, with)

getUsers :<|> postUsers :<|> getUsersId :<|> putUsersId :<|> deleteUsersId :<|> getUsersIdFollowing = client api

emptyToken = Just "emptyToken"

apiTests conf = testSpec "API Tests" (spec conf)

spec conf = do
  describe "API Tests" $ around (withApp conf) $ do
    context "/users" $ do
      it "returns an empty list" $ \host -> do
        try host (getUsers emptyToken) `shouldReturn` []

type Host = (Manager, BaseUrl)

try :: Host -> (Manager -> BaseUrl -> ClientM a) -> IO a
try (manager, baseUrl) action = do
  result <- runExceptT $ action manager baseUrl
  case result of
    Right x -> return x
    Left err -> throwIO $ ErrorCall $ show err

withApp :: (Manager, Fb.UserAccessToken, Fb.Credentials) -> (Host -> IO a) -> IO a
withApp conf action = testWithApplication (testApp conf) $ \ port -> do
  manager <- newManager defaultManagerSettings
  let url = BaseUrl Http "localhost" port ""
  action (manager, url)

testApp :: (Manager, Fb.UserAccessToken, Fb.Credentials) -> IO Application
testApp (_, u, c) = do
  let logSettings = Logger.Settings
        { Logger.filePath = "webserver.log"
        , Logger.logLevel = LevelDebug
        , Logger.noConsoleLogging = True
        }
  runNoLoggingT $ do
    let port = 3000 :: Int
    pool <- runNoLoggingT $ createSqlitePool ":memory:" 10
    runSqlPool (runMigrationSilent migrateAll) pool
    manager <- liftIO $ newManager tlsManagerSettings
    let env = LogEnv undefined $ HandlerEnv pool manager c
    return $ app env
