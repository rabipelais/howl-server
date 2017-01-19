{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module EventsApiTests where

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

eventsSpec = context "/events" $ do
  it "returns an empty list" $ \host -> do
    try host (getEvents emptyToken) `shouldReturn` []

  context "POST" $ do
    it "creates one new event" $ \host -> do
      try host (putEvents event1 emptyToken)
      es <- try host (getEvents emptyToken)
      es `shouldBe` [event1]

    it "is idempotent" $ \host -> do
      try host (putEvents event1 emptyToken)
      try host (putEvents event1 emptyToken)
      es <- try host (getEvents emptyToken)
      es `shouldBe` [event1]

    it "modifies an event" $ \host -> do
      try host (putEvents event1 emptyToken)
      let event' = event1 {eventName = "ASRT"}
      try host (putEvents event' emptyToken)
      es <- try host (getEvents emptyToken)
      es `shouldBe` [event']

    it "add two events" $ \host -> do
      try host (putEvents event1 emptyToken)
      try host (putEvents event2 emptyToken)
      es <- try host (getEvents emptyToken)
      es `shouldBe` [event1, event2]

    eventsIdSpec

eventsIdSpec = context "/events/{eventID}"$ do
  it "returns 404 if event doesn't exist" $ \(manager, baseUrl) -> do
    Left err <- runExceptT $ getEventsId event1Id emptyToken manager baseUrl
    responseStatus err `shouldBe` notFound404

  it "return the event in DB" $ \host -> do
    try host (putEvents event1 emptyToken)
    e <- try host (getEventsId event1Id emptyToken)
    e `shouldBe`event1

  eventsIdInviteSpec
  eventsIdRSVPSpec

eventsIdInviteSpec = context "/events/{eventID}/invites" $ do
  it "returns 404 if event does not exist" $ \(manager, baseUrl) -> do
    Left err <- runExceptT $ getEventsIdInvites event1Id emptyToken manager baseUrl
    responseStatus err `shouldBe` notFound404

  it "returns empty list" $ \host -> do
    try host (putUsers albert emptyToken)
    try host (putEvents event1 albertToken)
    r <- try host (getEventsIdInvites event1Id albertToken)
    r `shouldBe` []

  context "/events/{eventID}/invites/{friendID}" $ do
    context "GET" $ do
      it "returns 404 if event does not exist" $ \(manager, baseUrl) -> do
        Left err <- runExceptT $ getEventsIdInvitesId event1Id bobId emptyToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "returns 404 if friend does not exist" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putEvents event1 emptyToken)
        Left err <- runExceptT $ getEventsIdInvitesId event1Id bobId emptyToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "returns 401 if token does not name an existing user" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putEvents event1 emptyToken)
        try (manager, baseUrl) (putUsers bob emptyToken)
        Left err <- runExceptT $ getEventsIdInvitesId event1Id bobId albertToken manager baseUrl
        responseStatus err `shouldBe` unauthorized401

      it "returns 404 if user did not invite friend" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putEvents event1 emptyToken)
        try (manager, baseUrl) (putUsers albert emptyToken)
        try (manager, baseUrl) (putUsers bob emptyToken)
        Left err <- runExceptT $ getEventsIdInvitesId event1Id bobId albertToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "returns the invite" $ \host -> do
        try host (putUsers albert emptyToken)
        try host (putUsers bob emptyToken)
        try host (putEvents event1 albertToken)
        try host (postEventsIdInvitesId event1Id bobId albertToken)
        r <- try host (getEventsIdInvitesId event1Id bobId albertToken)
        r `shouldBe` (Invite albertId bobId event1Id)

    context "POST" $ do
      it "returns 404 if event does not exist" $ \(manager, baseUrl) -> do
        Left err <- runExceptT $ postEventsIdInvitesId event1Id bobId emptyToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "returns 404 if friend does not exist" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putEvents event1 emptyToken)
        Left err <- runExceptT $ postEventsIdInvitesId event1Id bobId emptyToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "returns 401 if token does not name an existing user" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putEvents event1 emptyToken)
        try (manager, baseUrl) (putUsers bob emptyToken)
        Left err <- runExceptT $ postEventsIdInvitesId event1Id bobId albertToken manager baseUrl
        responseStatus err `shouldBe` unauthorized401

      it "returns 409 if user tries to invite friend twice" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putEvents event1 emptyToken)
        try (manager, baseUrl) (putUsers albert emptyToken)
        try (manager, baseUrl) (putUsers bob emptyToken)
        try (manager, baseUrl) (postEventsIdInvitesId event1Id bobId albertToken)
        Left err <- runExceptT $ postEventsIdInvitesId event1Id bobId albertToken manager baseUrl
        responseStatus err `shouldBe` conflict409

      it "returns 409 if friend is not 'not_replied'" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putEvents event1 emptyToken)
        try (manager, baseUrl) (putUsers albert emptyToken)
        try (manager, baseUrl) (putUsers bob emptyToken)
        try (manager, baseUrl) (putEventsIdRSVPUsersId event1Id bobId Fb.Attending albertToken)
        Left err <- runExceptT $ postEventsIdInvitesId event1Id bobId albertToken manager baseUrl
        responseStatus err `shouldBe` conflict409

      it "is able to post an invite" $ \host -> do
        try host (putUsers albert emptyToken)
        try host (putUsers bob emptyToken)
        try host (putEvents event1 albertToken)
        try host (postEventsIdInvitesId event1Id bobId albertToken)
        r <- try host (getEventsIdInvitesId event1Id bobId albertToken)
        r `shouldBe` (Invite albertId bobId event1Id)

    context "DELETE" $ do
      it "returns 404 if event does not exist" $ \(manager, baseUrl) -> do
        Left err <- runExceptT $ deleteEventsIdInvitesId event1Id bobId emptyToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "returns 404 if friend does not exist" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putEvents event1 emptyToken)
        Left err <- runExceptT $ deleteEventsIdInvitesId event1Id bobId emptyToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "returns 401 if token does not name an existing user" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putEvents event1 emptyToken)
        try (manager, baseUrl) (putUsers bob emptyToken)
        Left err <- runExceptT $ deleteEventsIdInvitesId event1Id bobId albertToken manager baseUrl
        responseStatus err `shouldBe` unauthorized401

      it "returns 404 if user did not invite this friend" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putEvents event1 emptyToken)
        try (manager, baseUrl) (putUsers albert emptyToken)
        try (manager, baseUrl) (putUsers bob emptyToken)
        Left err <- runExceptT $ deleteEventsIdInvitesId event1Id bobId albertToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "deletes the invite" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putEvents event1 emptyToken)
        try (manager, baseUrl) (putUsers bob emptyToken)
        try (manager, baseUrl) (putUsers albert emptyToken)
        try (manager, baseUrl) (postEventsIdInvitesId event1Id bobId albertToken)
        try (manager, baseUrl) (deleteEventsIdInvitesId event1Id bobId albertToken)
        Left err <- runExceptT $ getEventsIdInvitesId event1Id bobId albertToken manager baseUrl
        responseStatus err `shouldBe` notFound404

eventsIdRSVPSpec = context "/events/{eventID}/rsvp" $ do
  it "returns 404 if event doesn't exist" $ \(manager, baseUrl) -> do
    Left err <- runExceptT $ getEventsIdRSVP event1Id albertToken manager baseUrl
    responseStatus err `shouldBe` notFound404

  it "returns 401 if the token doesn't belong to an existing user" $ \(manager, baseUrl) -> do
    try (manager, baseUrl) (putEvents event1 albertToken)
    Left err <- runExceptT $ getEventsIdRSVP event1Id albertToken manager baseUrl
    responseStatus err `shouldBe` unauthorized401

  it "returns an empty list" $ \host -> do
    try host (putUsers albert emptyToken)
    try host (putEvents event1 albertToken)
    rs <- try host (getEventsIdRSVP event1Id albertToken)
    rs `shouldBe` []

  context "/events/{eventID}/rsvp/{userID}" $ do
    context "GET" $ do
      it "returns 404 if event does not exist" $ \(manager, baseUrl) -> do
        Left err <- runExceptT $ getEventsIdRSVPUsersId event1Id bobId emptyToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "returns 404 if friend does not exist" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putEvents event1 emptyToken)
        Left err <- runExceptT $ getEventsIdRSVPUsersId event1Id bobId emptyToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "returns 401 if token does not name an existing user" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putEvents event1 emptyToken)
        try (manager, baseUrl) (putUsers bob emptyToken)
        Left err <- runExceptT $ getEventsIdRSVPUsersId event1Id bobId albertToken manager baseUrl
        responseStatus err `shouldBe` unauthorized401

      it "returns 404 if there's no RSVP for this event and this user" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putEvents event1 emptyToken)
        try (manager, baseUrl) (putUsers albert emptyToken)
        try (manager, baseUrl) (putUsers bob emptyToken)
        Left err <- runExceptT $ getEventsIdRSVPUsersId event1Id bobId albertToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "inviting sets an RSVP with 'NotReplied'" $ \host -> do
        try host (putUsers albert emptyToken)
        try host (putUsers bob emptyToken)
        try host (putEvents event1 albertToken)
        try host (postEventsIdInvitesId event1Id bobId albertToken)
        r <- try host (getEventsIdRSVPUsersId event1Id bobId albertToken)
        r `shouldBe` (EventRSVP bobId event1Id Fb.NotReplied)

    context "PUT" $ do
      it "returns 404 if event does not exist" $ \(manager, baseUrl) -> do
        Left err <- runExceptT $ putEventsIdRSVPUsersId event1Id bobId Fb.Maybe emptyToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "returns 404 if friend does not exist" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putEvents event1 emptyToken)
        Left err <- runExceptT $ putEventsIdRSVPUsersId event1Id bobId Fb.Declined emptyToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "returns 401 if token does not name an existing user" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putEvents event1 emptyToken)
        try (manager, baseUrl) (putUsers bob emptyToken)
        Left err <- runExceptT $ putEventsIdRSVPUsersId event1Id bobId Fb.Attending albertToken manager baseUrl
        responseStatus err `shouldBe` unauthorized401

      it "is able to put an RSVP" $ \host -> do
        try host (putUsers albert emptyToken)
        try host (putUsers bob emptyToken)
        try host (putEvents event1 albertToken)
        try host (putEventsIdRSVPUsersId event1Id bobId Fb.Created albertToken)
        r <- try host (getEventsIdRSVPUsersId event1Id bobId albertToken)
        r `shouldBe` (EventRSVP bobId event1Id Fb.Created)

      it "is idempotent" $ \host -> do
        try host (putUsers albert emptyToken)
        try host (putUsers bob emptyToken)
        try host (putEvents event1 albertToken)
        try host (putEventsIdRSVPUsersId event1Id bobId Fb.Created albertToken)
        try host (putEventsIdRSVPUsersId event1Id bobId Fb.Created albertToken)
        r <- try host (getEventsIdRSVPUsersId event1Id bobId albertToken)
        r `shouldBe` (EventRSVP bobId event1Id Fb.Created)

      it "can modify an RSVP" $ \host -> do
        try host (putUsers albert emptyToken)
        try host (putUsers bob emptyToken)
        try host (putEvents event1 albertToken)
        try host (putEventsIdRSVPUsersId event1Id bobId Fb.Created albertToken)
        try host (putEventsIdRSVPUsersId event1Id bobId Fb.NotReplied albertToken)
        r <- try host (getEventsIdRSVPUsersId event1Id bobId albertToken)
        r `shouldBe` (EventRSVP bobId event1Id Fb.NotReplied)

    context "DELETE" $ do
      it "returns 404 if event does not exist" $ \(manager, baseUrl) -> do
        Left err <- runExceptT $ deleteEventsIdRSVPUsersId event1Id bobId emptyToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "returns 404 if friend does not exist" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putEvents event1 emptyToken)
        Left err <- runExceptT $ deleteEventsIdRSVPUsersId event1Id bobId emptyToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "returns 401 if token does not name an existing user" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putEvents event1 emptyToken)
        try (manager, baseUrl) (putUsers bob emptyToken)
        Left err <- runExceptT $ deleteEventsIdRSVPUsersId event1Id bobId albertToken manager baseUrl
        responseStatus err `shouldBe` unauthorized401

      it "returns 404 friend had no RSVP" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putEvents event1 emptyToken)
        try (manager, baseUrl) (putUsers albert emptyToken)
        try (manager, baseUrl) (putUsers bob emptyToken)
        Left err <- runExceptT $ deleteEventsIdRSVPUsersId event1Id bobId albertToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "deletes the RSVP" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putEvents event1 emptyToken)
        try (manager, baseUrl) (putUsers bob emptyToken)
        try (manager, baseUrl) (putUsers albert emptyToken)
        try (manager, baseUrl) (postEventsIdInvitesId event1Id bobId albertToken)
        try (manager, baseUrl) (deleteEventsIdRSVPUsersId event1Id bobId albertToken)
        Left err <- runExceptT $ getEventsIdRSVPUsersId event1Id bobId albertToken manager baseUrl
        responseStatus err `shouldBe` notFound404
