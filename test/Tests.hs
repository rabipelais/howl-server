{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
import Test.Tasty
import Test.Tasty.HUnit

import Howl
import qualified Howl.Facebook as Fb

import Data.Text
import Data.Monoid ((<>))
import Data.Time.Clock
import Network.HTTP.Conduit (Manager, newManager, tlsManagerSettings)
import Control.Monad.Trans.Resource
import Data.Configurator as C
import Data.Configurator.Types

getCredentials :: FilePath -> IO (Maybe Fb.Credentials)
getCredentials filePath = do
  config <- load [Required filePath]
  appName <- C.lookup config "credentials.appName"
  appId <- C.lookup config "credentials.appId"
  appSecret <- C.lookup config "credentials.appSecret"
  return $ Fb.Credentials <$> appName <*> appId <*> appSecret

getCreds cfgPath token = do
  expires <- addUTCTime 400000000 <$> getCurrentTime
  manager <- newManager tlsManagerSettings
  let userAT = Fb.UserAccessToken "10155182179270463" token expires
  (Just creds) <- getCredentials cfgPath
  return (manager, userAT, creds)

main :: IO ()
main = do
  testCfg <- load [Required "tests.cfg"]
  (Just config) <- C.lookup testCfg "test.configPath"
  (Just token) <- C.lookup testCfg "test.token"
  creds <- getCreds config token
  defaultMain (suite creds testCfg)

suite creds testCfg = testGroup "Test Suite"
  [ (userTests creds testCfg)
  , (eventsTests creds testCfg)]

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
      return $ User fbID username firstName lastName email


instance Configured (Maybe Text) where
  convert (String v) = case first of
    "Just" -> Just (Just second)
    _  -> Just Nothing
    where (first : second : _) = Data.Text.words v
  convert _ = Nothing

instance Configured IDType where
  convert (String v) = Just (Fb.Id v)
  convert _ = Nothing


eventsTests (m, u, c) testCfg =
  testGroup "Events tsts"
  [ testCase "Get events" $ do
      (fbEvents :: Fb.Pager Fb.Event) <- runResourceT $ Fb.runFacebookT c m $ Fb.getObject ("/v2.8/" <> "10155182179270463" <> "/" <> "events") [("fields", "id,name,category,description,start_time,end_time,place,rsvp_status,owner")] (Just u)
      print fbEvents
  ]
