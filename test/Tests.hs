{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

import           Test.Tasty
import           Test.Tasty.Hspec
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

import           ApiTests

getCredentials :: FilePath -> IO (Maybe Fb.Credentials)
getCredentials filePath = do
  config <- load [Required filePath]
  appName <- C.lookup config "credentials.appName"
  appId <- C.lookup config "credentials.appId"
  appSecret <- C.lookup config "credentials.appSecret"
  return $ Fb.Credentials <$> appName <*> appId <*> appSecret

makeConf cfgPath token = do
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
  conf <- makeConf config token
  apiTestTree <- apiTests conf
  putStrLn "Starting tests"
  defaultMain (testGroup "Test Suite" [apiTestTree])


instance Configured (Maybe Text) where
  convert (String v) = case first of
    "Just" -> Just (Just second)
    _  -> Just Nothing
    where (first : second : _) = Data.Text.words v
  convert _ = Nothing

instance Configured IDType where
  convert (String v) = Just (Fb.Id v)
  convert _ = Nothing
