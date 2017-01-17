{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module UsersApiTests where

import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit

import           Howl
import qualified Howl.Facebook                as Fb
import qualified Howl.Logger                  as Logger
import           TestsCommon

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


getUsers :<|> postUsers :<|> putUsers :<|> getUsersId :<|> putUsersId :<|> deleteUsersId :<|> getUsersIdFollows :<|> postUsersIdFollows :<|> getUsersIdFollowsId :<|> deleteUsersIdFollowsId :<|> getUsersIdBlocked :<|> postUsersIdBlocked :<|> deleteUsersIdBlockedId :<|> getUsersIdEvents :<|> getUsersIdEventsFollows = client api


-- ============= USERS SPEC ===================================
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
        usersIdEventsSpec
        usersIdBlockedSpec
-- //USERS ID SPEC

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
            Left err <- runExceptT $ postUsersIdFollows "12345" "12346" emptyToken manager baseUrl
            responseStatus err `shouldBe` notFound404

          it "returns 404 for non-existent target user" $ \(manager, baseUrl) -> do
            try (manager, baseUrl) (putUsers albert emptyToken)
            Left err <- runExceptT $ postUsersIdFollows "12345" "12346" emptyToken manager baseUrl
            responseStatus err `shouldBe` notFound404

          it "returns 409 if users tries to follow themselves" $ \(manager, baseUrl) -> do
            try (manager, baseUrl) (putUsers albert emptyToken)
            Left err <- runExceptT $ postUsersIdFollows "12345" "12345" emptyToken manager baseUrl
            responseStatus err `shouldBe` status409

          it "returns 409 if source is alredy following target" $ \(manager, baseUrl) -> do
            try (manager, baseUrl) (putUsers albert emptyToken)
            Left err <- runExceptT $ postUsersIdFollows "12345" "12345" emptyToken manager baseUrl
            responseStatus err `shouldBe` conflict409

          it "returns 403 if users tries to follow user who blocked them" $ \(manager, baseUrl) -> do
            try (manager, baseUrl) (putUsers albert emptyToken)
            try (manager, baseUrl) (putUsers bob emptyToken)
            try (manager, baseUrl) (postUsersIdBlocked bobId albertId emptyToken)
            Left err <- runExceptT $ postUsersIdFollows "12345" "67890" emptyToken manager baseUrl
            responseStatus err `shouldBe` status403

          it "marks the user as following the target" $ \host -> do
            try host (putUsers albert emptyToken)
            try host (putUsers bob emptyToken)
            try host (postUsersIdFollows "12345" "67890" emptyToken)
            fs <- try host (getUsersIdFollows "12345" emptyToken)
            fs `shouldBe` [bob]

        usersIdFollowsIdSpec
-- // USERS ID FOLLOW

usersIdFollowsIdSpec = context "/users/{userID}/follows/{targetID}" $ do
  context "GET" $ do
    it "returns 404 for non-existent requesting user" $ \(manager, baseUrl) -> do
            Left err <- runExceptT $ getUsersIdFollowsId "12345" "12345" emptyToken manager baseUrl
            responseStatus err `shouldBe` notFound404

    it "returns 404 for non-existent target user" $ \(manager, baseUrl) -> do
            try (manager, baseUrl) (putUsers albert emptyToken)
            Left err <- runExceptT $ getUsersIdFollowsId "12345" "12346" emptyToken manager baseUrl
            responseStatus err `shouldBe` notFound404

    it "returns 404 if source is not following target" $ \(manager, baseUrl) -> do
            try (manager, baseUrl) (putUsers albert emptyToken)
            try (manager, baseUrl) (putUsers bob emptyToken)
            Left err <- runExceptT $ getUsersIdFollowsId "12345" "67890" emptyToken manager baseUrl
            responseStatus err `shouldBe` notFound404

    it "returns Accepted if source is following target" $ \host -> do
            try host (putUsers albert emptyToken)
            try host (putUsers bob emptyToken)
            try host (postUsersIdFollows "12345" "67890" emptyToken)
            fs <- try host (getUsersIdFollowsId "12345" "67890" emptyToken)
            fs `shouldBe` Accepted

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
-- USER ID FOLLOW ID

usersIdEventsSpec = context "/users/{userID}/events" $ do
  it "return 404 if user doesn't exist" $ \(manager, baseUrl) -> do
    Left err <- runExceptT $ getUsersIdEvents "12345" emptyToken manager baseUrl
    responseStatus err `shouldBe` notFound404

  it "returns empty list" $ \host -> do
    try host (putUsers albert emptyToken)
    es <- try host (getUsersIdEvents "12345" emptyToken)
    es `shouldBe` []

  it "returns the user's events list" $ \host -> do
    pending

  usersIdEventsFollowsSpec
-- USERS ID EVENTS

usersIdBlockedSpec = context "/users/{userID}/blocked" $ do
  it "returns 404 if user doesn't exist" $ \(manager, baseUrl) -> do
    Left err <- runExceptT $ getUsersIdBlocked "12345" emptyToken manager baseUrl
    responseStatus err `shouldBe` notFound404

  it "returns an empty list" $ \host -> do
    try host (putUsers albert emptyToken)
    bs <- try host (getUsersIdBlocked "12345" emptyToken)
    bs `shouldBe` []

  context "POST" $ do
    it "returns 404 if user doesn't exist" $ \(manager, baseUrl) -> do
      Left err <- runExceptT $ postUsersIdBlocked "12345" "12346" emptyToken manager baseUrl
      responseStatus err `shouldBe` notFound404

    it "returns 404 if target user doesn't exist" $ \(manager, baseUrl) -> do
      try (manager, baseUrl) (putUsers albert emptyToken)
      Left err <- runExceptT $ postUsersIdBlocked "12345" "12346" emptyToken manager baseUrl
      responseStatus err `shouldBe` notFound404

    it "returns 409 if user tries to block themself" $ \(manager, baseUrl) -> do
      try (manager, baseUrl) (putUsers albert emptyToken)
      Left err <- runExceptT $ postUsersIdBlocked "12345" "12345" emptyToken manager baseUrl
      responseStatus err `shouldBe` conflict409

    it "returns 409 if target is already blocked" $ \(manager, baseUrl) -> do
      try (manager, baseUrl) (putUsers albert emptyToken)
      try (manager, baseUrl) (putUsers bob emptyToken)
      try (manager, baseUrl) (postUsersIdBlocked "12345" "67890" emptyToken)
      Left err <- runExceptT $ postUsersIdBlocked "12345" "67890" emptyToken manager baseUrl
      responseStatus err `shouldBe` conflict409

    it "marks targets as blocked" $ \host -> do
      try host (putUsers albert emptyToken)
      try host (putUsers bob emptyToken)
      try host (putUsers charles emptyToken)
      try host (postUsersIdBlocked "12345" bobId emptyToken)
      try host (postUsersIdBlocked "12345" charlesId emptyToken)
      bs <- try host (getUsersIdBlocked "12345" emptyToken)
      bs `shouldBe` [charles, bob]

    it "marks target as blocked, and removes s->t follow" $ \host -> do
      try host (putUsers albert emptyToken)
      try host (putUsers bob emptyToken)
      try host (postUsersIdFollows albertId bobId emptyToken)
      try host (postUsersIdBlocked albertId bobId emptyToken)
      fs <- try host (getUsersIdFollows albertId emptyToken)
      fs `shouldBe` []
      bs <- try host (getUsersIdBlocked albertId emptyToken)
      bs `shouldBe` [bob]

    it "makes target stop following source after block" $ \host -> do
      try host (putUsers albert emptyToken)
      try host (putUsers bob emptyToken)
      try host (postUsersIdFollows albertId bobId emptyToken)
      fs <- try host (getUsersIdFollows albertId emptyToken)
      fs `shouldBe` [bob]
      try host (postUsersIdBlocked bobId albertId emptyToken)
      fs <- try host (getUsersIdFollows albertId emptyToken)
      fs `shouldBe` []

  context "/users/{userID}/blocked/{friendID}" $ do
    context "DELETE" $ do
      it "returns 404 if user doesn't exist" $ \(manager, baseUrl) -> do
        Left err <- runExceptT $ deleteUsersIdBlockedId "12345" "12346" emptyToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "returns 404 if target user doesn't exist" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putUsers albert emptyToken)
        Left err <- runExceptT $ deleteUsersIdBlockedId "12345" "12346" emptyToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "returns 404 if target is not blocked" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putUsers albert emptyToken)
        try (manager, baseUrl) (putUsers bob emptyToken)
        Left err <- runExceptT $ deleteUsersIdBlockedId albertId bobId emptyToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "deletes the blocked status" $ \host -> do
        try host (putUsers albert emptyToken)
        try host (putUsers bob emptyToken)
        try host (postUsersIdBlocked albertId bobId emptyToken)
        try host (deleteUsersIdBlockedId albertId bobId emptyToken)
        bs <- try host (getUsersIdBlocked albertId emptyToken)
        bs `shouldBe` []
-- USERS ID BLOCKED

usersIdEventsFollowsSpec = context "/users/{userID}/events/follows" $ do
  it "return 404 if user doesn't exist" $ \(manager, baseUrl) -> do
    Left err <- runExceptT $ getUsersIdEventsFollows "12345" emptyToken manager baseUrl
    responseStatus err `shouldBe` notFound404

  it "returns empty list" $ \host -> do
    try host (putUsers albert emptyToken)
    es <- try host (getUsersIdEventsFollows "12345" emptyToken)
    es `shouldBe` []

  it "returns the user's events list" $ \host -> do
    pending
-- USERS ID EVENTS FOLLOW