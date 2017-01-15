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

getUsers :<|> postUsers :<|> putUsers :<|> getUsersId :<|> putUsersId :<|> deleteUsersId :<|> getUsersIdFollows :<|> postUsersIdFollows :<|> getUsersIdFollowsId :<|> deleteUsersIdFollowsId :<|> getUsersIdFollowsEvents :<|> getUsersIdEvents = client api

emptyToken = Just "emptyToken"

apiTests conf = testSpec "API Tests" (spec conf)

spec conf = do
  describe "API Tests" $ around (withApp conf) $ do
    usersSpec

usersSpec =
  context "/users" $ do
      it "returns an empty list" $ \host -> do
        try host (getUsers emptyToken) `shouldReturn` []

      context "PUT" $ do
        it "creates one new user entry" $ \host -> do
          try host (putUsers albert emptyToken)
          u <- try host (getUsers emptyToken)
          u `shouldBe` [albert]

        it "is idempotent" $ \host -> do
          try host (putUsers albert emptyToken)
          try host (putUsers albert emptyToken)
          u <- try host (getUsers emptyToken)
          u `shouldBe` [albert]

        it "adds two users" $ \host -> do
          try host (putUsers albert emptyToken)
          try host (putUsers bob emptyToken)
          u <- try host (getUsers emptyToken)
          u `shouldBe` [albert, bob]

      usersIdSpec

usersIdSpec =
  context "/users/{userID}" $ do
        context "GET" $ do
          it "returns 404 for missing user" $ \(manager, baseUrl) -> do
            Left err <- runExceptT $ getUsersId "12345" emptyToken manager baseUrl
            responseStatus err `shouldBe` notFound404

          it "returns user in DB" $ \host -> do
            try host (putUsers albert emptyToken)
            u <- try host (getUsersId "12345" emptyToken)
            u `shouldBe` albert

        context "PUT" $ do
          it "returns 403 if replacing wrong user" $ \(manager, baseUrl) -> do
            Left err <- runExceptT $ putUsersId "12346" albert emptyToken manager baseUrl
            responseStatus err `shouldBe` forbidden403

          it "returns 404 if user not found"  $ \(manager, baseUrl) -> do
            Left err <- runExceptT $ putUsersId "12345" albert emptyToken manager baseUrl
            responseStatus err `shouldBe` notFound404

          it "modifies existing user" $ \host -> do
            try host (putUsers albert emptyToken)
            try host (putUsersId "12345" (albert{userFirstName = "Albertote"}) emptyToken)
            u <- try host (getUsersId "12345" emptyToken)
            u `shouldBe` (albert{userFirstName = "Albertote"})

          it "is idempotent" $ \host -> do
            try host (putUsers albert emptyToken)
            try host (putUsersId "12345" albert emptyToken)
            try host (putUsersId "12345" albert emptyToken)
            u <- try host (getUsers emptyToken)
            u `shouldBe` [albert]

        context "DELETE" $ do
          it "returns 404 when user not found" $ \(manager, baseUrl) -> do
            Left err <- runExceptT $ deleteUsersId "12345" emptyToken manager baseUrl
            responseStatus err `shouldBe` notFound404

          it "successfully deletes correct user" $ \host -> do
            try host (putUsers albert emptyToken)
            try host (deleteUsersId "12345" emptyToken)
            u <- try host (getUsers emptyToken)
            u `shouldBe` []

        usersIdFollowsSpec

usersIdFollowsSpec =
  context "users/{userID}/follows" $ do
        it "returns empty list" $ \host -> do
          try host (putUsers albert emptyToken)
          fs <- try host (getUsersIdFollows "12345" emptyToken)
          fs `shouldBe` []

        it "returns 404 for non-existing user" $ \(manager, baseUrl) -> do
          Left err <- runExceptT $ getUsersIdFollows "12345" emptyToken manager baseUrl
          responseStatus err `shouldBe` notFound404

        context "POST" $ do
          it "returns 404 for non-existent requesting user" $ \(manager, baseUrl) -> do
            Left err <- runExceptT $ postUsersIdFollows "12345" "12345" emptyToken manager baseUrl
            responseStatus err `shouldBe` notFound404

          it "returns 404 for non-existent target user" $ \(manager, baseUrl) -> do
            try (manager, baseUrl) (putUsers albert emptyToken)
            Left err <- runExceptT $ postUsersIdFollows "12345" "12346" emptyToken manager baseUrl
            responseStatus err `shouldBe` notFound404

          it "returns 409 if users tries to follow themselves" $ \(manager, baseUrl) -> do
            try (manager, baseUrl) (putUsers albert emptyToken)
            Left err <- runExceptT $ postUsersIdFollows "12345" "12345" emptyToken manager baseUrl
            responseStatus err `shouldBe` conflict409

          it "returns 403 if users tries to follow user who blocked them" $ \(manager, baseUrl) -> do
            try (manager, baseUrl) (putUsers albert emptyToken)
            try (manager, baseUrl) (putUsers bob emptyToken)
            Left err <- runExceptT $ postUsersIdFollows "12345" "67890" emptyToken manager baseUrl
            --responseStatus err `shouldBe` conflict409
            pending

          it "marks the user as following the target" $ \host -> do
            try host (putUsers albert emptyToken)
            try host (putUsers bob emptyToken)
            try host (postUsersIdFollows "12345" "67890" emptyToken)
            fs <- try host (getUsersIdFollows "12345" emptyToken)
            fs `shouldBe` [bob]

        usersIdFollowsIdSpec

usersIdFollowsIdSpec = context "/users/{userID}/follows/{targetID}" $ do
  context "DELETE" $ do
          it "returns 404 for non-existent requesting user" $ \(manager, baseUrl) -> do
            Left err <- runExceptT $ deleteUsersIdFollowsId "12345" "12345" emptyToken manager baseUrl
            responseStatus err `shouldBe` notFound404

          it "returns 404 for non-existent target user" $ \(manager, baseUrl) -> do
            try (manager, baseUrl) (putUsers albert emptyToken)
            Left err <- runExceptT $ deleteUsersIdFollowsId "12345" "12346" emptyToken manager baseUrl
            responseStatus err `shouldBe` notFound404

          it "returns 404 if source is not following target" $ \(manager, baseUrl) -> do
            try (manager, baseUrl) (putUsers albert emptyToken)
            try (manager, baseUrl) (putUsers bob emptyToken)
            Left err <- runExceptT $ deleteUsersIdFollowsId "12345" "67890" emptyToken manager baseUrl
            responseStatus err `shouldBe` notFound404

          it "successfully deletes follow status" $ \host -> do
            try host (putUsers albert emptyToken)
            try host (putUsers bob emptyToken)
            try host (postUsersIdFollows "12345" "67890" emptyToken)
            try host (deleteUsersIdFollowsId "12345" "67890" emptyToken)
            fs <- try host (getUsersIdFollows "12345" emptyToken)
            fs `shouldBe` []


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

albert :: User
albert =
  User "12345" "el boleta" "Albert" (Just "Boleta") (Just "albert@yahoo.co") Nothing

bob :: User
bob =
  User "67890" "abreu" "Bob" (Just "Patiño") (Just "bob@yahoo.de") Nothing

charles :: User
charles = User "1683671673" "charlie" "Charles" (Just "Flummoxon III") (Just "c.daddy@yahoo.de") Nothing
