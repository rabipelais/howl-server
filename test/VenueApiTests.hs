{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module VenueApiTests where

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

venuesSpec = context "/venues" $ do
  it "returns an empty list" $ \host -> do
    try host (getVenues emptyToken) `shouldReturn` []

  context "PUT" $ do
    it "creates a new venue" $ \host -> do
      try host (putVenues venue1 emptyToken)
      vs <- try host (getVenues emptyToken)
      vs `shouldBe` [venue1]

    it "is idempotent" $ \host -> do
      try host (putVenues venue1 emptyToken)
      try host (putVenues venue1 emptyToken)
      vs <- try host (getVenues emptyToken)
      vs `shouldBe` [venue1]

    it "modifies an event" $ \host -> do
      try host (putVenues venue1 emptyToken)
      let venue' = venue1 {venueName = "ARST"}
      try host (putVenues venue' emptyToken)
      vs <- try host (getVenues emptyToken)
      vs `shouldBe` [venue']

    it "add two venues" $ \host -> do
      try host (putVenues venue1 emptyToken)
      try host (putVenues venue2 emptyToken)
      vs <- try host (getVenues emptyToken)
      vs `shouldBe` [venue1, venue2]

    venuesIdSpec

venuesIdSpec = context "/venues/{venueID}" $ do
  it "returns 404 if venue doesn't exist" $ \(manager, baseUrl) -> do
    Left err <- runExceptT $ getVenuesId venue1Id emptyToken manager baseUrl
    responseStatus err `shouldBe` notFound404

  it "return the venue in DB" $ \host -> do
    try host (putVenues venue1 emptyToken)
    v <- try host (getVenuesId venue1Id emptyToken)
    v `shouldBe`venue1

  venuesIdFollowersSpec

venuesIdFollowersSpec = context "/venues/{venueID}/followers" $ do
  it "returns 404 if venue does not exist" $ \(manager, baseUrl) -> do
    Left err <- runExceptT $ getVenuesIdFollowers venue1Id emptyToken manager baseUrl
    responseStatus err `shouldBe` notFound404

  it "returns empty list" $ \host -> do
    try host (putUsers albert emptyToken)
    try host (putVenues venue1 albertToken)
    r <- try host (getVenuesIdFollowers venue1Id albertToken)
    r `shouldBe` []

  context "/venues/{venueID}/followers/{friendID}" $ do
    context "GET" $ do
      it "returns 404 if venue does not exist" $ \(manager, baseUrl) -> do
        Left err <- runExceptT $ getVenuesIdFollowersId venue1Id bobId emptyToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "returns 404 if friend does not exist" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putVenues venue1 emptyToken)
        Left err <- runExceptT $ getVenuesIdFollowersId venue1Id bobId emptyToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "returns 401 if token does not name an existing user" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putVenues venue1 emptyToken)
        try (manager, baseUrl) (putUsers bob emptyToken)
        Left err <- runExceptT $ getVenuesIdFollowersId venue1Id bobId albertToken manager baseUrl
        responseStatus err `shouldBe` unauthorized401

      it "returns the follower datatype if user is a follower" $ \host -> do
        try host (putUsers albert emptyToken)
        try host (putVenues venue1 albertToken)
        try host (putVenuesIdFollowersId venue1Id albertId albertToken)
        r <- try host (getVenuesIdFollowersId venue1Id albertId albertToken)
        r `shouldBe` (VenueFollower venue1Id albertId)

    context "POST" $ do
      it "returns 404 if venue does not exist" $ \(manager, baseUrl) -> do
        Left err <- runExceptT $ putVenuesIdFollowersId venue1Id bobId emptyToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "returns 404 if user does not exist" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putVenues venue1 emptyToken)
        Left err <- runExceptT $ putVenuesIdFollowersId venue1Id bobId emptyToken manager baseUrl
        responseStatus err `shouldBe` notFound404

      it "returns 401 if token does not name an existing user" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putVenues venue1 emptyToken)
        try (manager, baseUrl) (putUsers bob emptyToken)
        Left err <- runExceptT $ putVenuesIdFollowersId venue1Id bobId albertToken manager baseUrl
        responseStatus err `shouldBe` unauthorized401

      it "returns 403 if user and token don't match" $ \(manager, baseUrl) -> do
        try (manager, baseUrl) (putVenues venue1 emptyToken)
        try (manager, baseUrl) (putUsers albert emptyToken)
        try (manager, baseUrl) (putUsers bob emptyToken)
        Left err <- runExceptT $ putVenuesIdFollowersId venue1Id bobId albertToken manager baseUrl
        responseStatus err `shouldBe` forbidden403

      it "is able to put the follow" $ \host -> do
        try host (putUsers albert emptyToken)
        try host (putVenues venue1 albertToken)
        try host (putVenuesIdFollowersId venue1Id albertId albertToken)
        r <- try host (getVenuesIdFollowersId venue1Id albertId albertToken)
        r `shouldBe` (VenueFollower venue1Id albertId)

      it "is idempotent" $ \host -> do
        try host (putUsers albert emptyToken)
        try host (putVenues venue1 albertToken)
        try host (putVenuesIdFollowersId venue1Id albertId albertToken)
        try host (putVenuesIdFollowersId venue1Id albertId albertToken)
        r <- try host (getVenuesIdFollowersId venue1Id albertId albertToken)
        r `shouldBe` (VenueFollower venue1Id albertId)
