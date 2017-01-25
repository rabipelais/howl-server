{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
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
import           Data.Time.Calendar
import           Data.Time.Clock

import           Network.Wai                  (Application)
import           Network.Wai.Handler.Warp     as Warp
import           Servant.API
import           Servant.Client

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Control.Exception            (ErrorCall (..), throwIO)
import qualified Control.Exception.Lifted as E
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Resource
import           Data.Monoid                  ((<>))
import           Network.HTTP.Client          (Manager(..), defaultManagerSettings,
                                               newManager)
import           Network.HTTP.Conduit         (tlsManagerSettings, ManagerSettings(..))
import           Network.HTTP.Types
import           Test.Hspec                   (Spec, describe, hspec, it)
import           Test.Hspec.Wai               (WaiExpectation, WaiSession,
                                               delete, get, matchBody, request,
                                               shouldRespondWith, with)


(getUsers
  :<|> postUsers
  :<|> putUsers
  :<|> getUsersId
  :<|> putUsersId
  :<|> deleteUsersId
  :<|> getUsersIdFollows
  :<|> postUsersIdFollows
  :<|> getUsersIdFollowsId
  :<|> deleteUsersIdFollowsId
  :<|> getUsersIdBlocked
  :<|> postUsersIdBlocked
  :<|> deleteUsersIdBlockedId
  :<|> getUsersIdEvents
  :<|> getUsersIdEventsFollows
  :<|> getUsersIdVenues) :<|>
  (getEvents
  :<|> putEvents
  :<|> getEventsNearby
  :<|> getEventsId
  :<|> getEventsIdInvites
  :<|> getEventsIdInvitesId
  :<|> postEventsIdInvitesId
  :<|> deleteEventsIdInvitesId
  :<|> getEventsIdRSVP
  :<|> getEventsIdRSVPUsersId
  :<|> putEventsIdRSVPUsersId
  :<|> deleteEventsIdRSVPUsersId
  ) :<|>
  (getVenues
  :<|> putVenues
  :<|> getVenuesNearby
  :<|> getVenuesId
  :<|> getVenuesIdFollowers
  :<|> getVenuesIdFollowersId
  :<|> putVenuesIdFollowersId
  :<|> deleteVenuesIdFollowersId
  :<|> getVenuesIdEvents) = client api

emptyToken :: Maybe Text
emptyToken = Just "emptyToken"

albertToken :: Maybe Text
albertToken = Just "12345"

bobToken :: Maybe Text
bobToken = Just (Fb.idCode bobId)

type Host = (Manager, BaseUrl)

try :: Host -> (Manager -> BaseUrl -> ClientM a) -> IO a
try (manager, baseUrl) action = do
  result <- runExceptT $ action manager baseUrl
  case result of
    Right x -> return x
    Left err -> throwIO $ ErrorCall $ show err

withApp :: (Manager, Fb.UserAccessToken, Fb.Credentials) -> (Host -> IO a) -> IO a
withApp conf action = testWithApplication (testApp conf) $ \ port -> do
  let settings = tlsManagerSettings
  manager <- newManager settings{managerResponseTimeout = Nothing}
  let url = BaseUrl Http "localhost" port ""
  action (manager, url)

testApp :: (Manager, Fb.UserAccessToken, Fb.Credentials) -> IO Application
testApp (_, u, c) = do
  let logSettings = Logger.Settings
        { Logger.filePath = "webserver-tests.log"
        , Logger.logLevel = LevelDebug
        , Logger.noConsoleLogging = True
        }
  Logger.withLogger logSettings $ \logFn -> do
    let port = 3000 :: Int
    pool <- runNoLoggingT $ createSqlitePool ":memory:" 10
    runSqlPool (runMigrationSilent migrateAll) pool
    manager <- liftIO $ newManager tlsManagerSettings{managerResponseTimeout = Nothing}
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

event1Id :: IDType
event1Id = "12345"

venue1Id :: IDType
venue1Id = "12345"

event1 = Event event1Id
  "Fun fun time"
  "The greatest event ever"
  (UTCTime (fromGregorian 2017 1 17) (secondsToDiffTime 40000))
  (UTCTime (fromGregorian 2017 1 18) (secondsToDiffTime 40000))
  venue1Id
  (Just "www.coolpic.gov")

event2Id :: IDType
event2Id = "67890"

venue2Id :: IDType
venue2Id = "67890"

event2 = Event event2Id
  "Not that fun of a time"
  "The kinda cool event"
  (UTCTime (fromGregorian 2017 2 17) (secondsToDiffTime 40000))
  (UTCTime (fromGregorian 2017 2 18) (secondsToDiffTime 40000))
  venue2Id
  (Just "www.supercoolpicsite.co.uk")

venue1 = Venue venue1Id
  Nothing
  "The freshest venue on the East Side"
  "Swag-town"
  "Left of here"
  Nothing Nothing
  (Just 4)

venue2 = Venue venue2Id
  Nothing
  "Westside is best side"
  "Le ouest"
  "Second star to the right"
  Nothing Nothing
  (Just 2)


-- | Perform an action with a new test user. Remove the new test user
-- after the action is performed.
withTestUser :: (MonadResource m, MonadBaseControl IO m)
                => Fb.CreateTestUser
                -> (Fb.TestUser -> Fb.FacebookT Fb.Auth m a)
                -> Fb.FacebookT Fb.Auth m a
withTestUser ctu action = do
  token <- Fb.getAppAccessToken
  E.bracket (Fb.createTestUser ctu token)
            (flip Fb.removeTestUser token)
            action

getTestToken testUser = do
  appToken <- Fb.getAppAccessToken
  Just testUserAccessTokenData <- return (Fb.tuAccessToken testUser)
  ret <- Fb.debugToken appToken testUserAccessTokenData
  return $ Fb.dtAccessToken ret

-- Wrappers for HUnit operators using MonadIO
(&?=) :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
v &?= e = liftIO (v @?= e)

(#?=) :: (Eq a, Show a, MonadIO m) => m a -> a -> m ()
m #?= e = m >>= (&?= e)
