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
