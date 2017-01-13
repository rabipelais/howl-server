{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Tests.FbTests where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Howl
import qualified Howl.Facebook                as Fb

import           Control.Monad.Trans.Resource
import           Data.Configurator            as C
import           Data.Configurator.Types
import           Data.Monoid                  ((<>))
import           Data.Text
import           Data.Time.Clock
import           Network.HTTP.Conduit         (Manager, newManager,
                                               tlsManagerSettings)

facebookTests creds testCfg = testGroup "Facebook Tests"
  [ (userTests creds testCfg)
  , (tokenTests creds)
  , (eventsTests creds testCfg)]

tokenTests :: (Manager, Fb.UserAccessToken, Fb.Credentials) -> TestTree
tokenTests (m, u@(Fb.UserAccessToken _ t e), c) =
  testGroup "Token tests"
  [ testCase "Valid token is valid" $ do
      bool <- runResourceT $ Fb.runFacebookT c m $ Fb.isValid u
      assertBool "" bool
  , testCase "Invalid token is invalid" $ do
      bool <- runResourceT $ Fb.runFacebookT c m $ Fb.isValid (Fb.UserAccessToken "125823897917914" t e)
      print bool
      assertBool "" (not bool)
  ]

userTests (m, u, c) testCfg =
    testGroup "User tests"
    [ testCase "Getting user" $ do
        sebas <- sebasIO
        newUser <- runResourceT $ getNewUser u c m
        assertEqual "" sebas newUser
    ]
  where
    sebasIO = getUserCfg testCfg
    getUserCfg config = do
      (Just fbID) <- C.lookup config "test.sebas.fbID"
      (Just username) <- C.lookup config "test.sebas.username"
      (Just firstName) <- C.lookup config "test.sebas.firstName"
      (Just lastName) <- C.lookup config "test.sebas.lastName"
      (Just email) <- C.lookup config "test.sebas.email"
      return $ User fbID username firstName lastName email Nothing



eventsTests (m, u, c) testCfg =
  testGroup "Events tsts"
  [ testCase "Get events" $ do
      (fbEvents :: Fb.Pager Fb.Event) <- runResourceT $ Fb.runFacebookT c m $ Fb.getObject ("/v2.8/" <> "10155182179270463" <> "/" <> "events") [("fields", "id,name,category,description,start_time,end_time,place,rsvp_status,owner")] (Just u)
      return ()
  ]
