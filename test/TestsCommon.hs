{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module TestsCommon where

import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit

import           Howl
import qualified Howl.Facebook                as Fb
import qualified Howl.Logger                  as Logger

import           Data.Text

import           Network.Wai                  (Application)
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
import           Network.HTTP.Client          (Manager, defaultManagerSettings,
                                               newManager)
import           Network.HTTP.Conduit         (tlsManagerSettings)
import           Network.HTTP.Types
import           Test.Hspec                   (Spec, describe, hspec, it)
import           Test.Hspec.Wai               (WaiExpectation, WaiSession,
                                               delete, get, matchBody, request,
                                               shouldRespondWith, with)

emptyToken :: Maybe Text
emptyToken = Just "emptyToken"

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
        , Logger.noConsoleLogging = False
        }
  Logger.withLogger logSettings $ \logFn -> do
    let port = 3000 :: Int
    pool <- runNoLoggingT $ createSqlitePool ":memory:" 10
    runSqlPool (runMigrationSilent migrateAll) pool
    manager <- liftIO $ newManager tlsManagerSettings
    let env = LogEnv logFn $ noAuthHandlerEnv pool manager c
    return $ app env

albert :: User
albert =
  User "12345" "el boleta" "Albert" (Just "Boleta") (Just "albert@yahoo.co") Nothing

albertId :: Fb.Id
albertId = "12345"

bob :: User
bob =
  User "67890" "abreu" "Bob" (Just "Patiño") (Just "bob@yahoo.de") Nothing

bobId :: Fb.Id
bobId = "67890"

charles :: User
charles = User "1683671673" "charlie" "Charles" (Just "Flummoxon III") (Just "c.daddy@yahoo.de") Nothing

charlesId :: Fb.Id
charlesId = "1683671673"