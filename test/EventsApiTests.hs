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

eventsIdInviteSpec = context "/events/{eventID}/invite" $ do
  it "returns 404 if event does not exist" $ \(manager, baseUrl) -> do
    Left err <- runExceptT $ getEventsIdInvites event1Id emptyToken manager baseUrl
    responseStatus err `shouldBe` notFound404

  it "returns empty list" $ \host -> do
    try host (putUsers albert emptyToken)
    try host (putEvents event1 albertToken)
    r <- try host (getEventsIdInvites event1Id albertToken)
    r `shouldBe` []
