{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

import           System.Environment           (getEnv)
import           System.Exit                  (exitFailure)
import           System.IO.Error              (isDoesNotExistError)
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit

import           Howl
import qualified Howl.Facebook                as Fb

import qualified Control.Exception.Lifted     as E
import           Control.Monad.Trans.Resource
import           Data.Configurator            as C
import           Data.Configurator.Types
import           Data.Monoid                  ((<>))
import           Data.Text                    as T
import           Data.Time.Clock
import           Network.HTTP.Conduit         (Manager, newManager,
                                               tlsManagerSettings)

import           ApiTests
import           FacebookTests                (facebookSpecTests)

getCredentials :: IO  Fb.Credentials
getCredentials = tryToGet `E.catch` showHelp
    where
      tryToGet = do
        [appName, appId, appSecret] <- mapM getEnv ["APP_NAME", "APP_ID", "APP_SECRET"]
        return $ Fb.Credentials (T.pack appName) (T.pack appId) (T.pack appSecret)

      showHelp exc | not (isDoesNotExistError exc) = E.throw exc
      showHelp _ = do
        putStrLn $ Prelude.unlines
          [ "In order to run the tests from the 'fb' package, you need"
          , "developer access to a Facebook app.  The tests are designed"
          , "so that your app isn't going to be hurt, but we may not"
          , "create a Facebook app for this purpose and then distribute"
          , "its secret keys in the open."
          , ""
          , "Please give your app's name, id and secret on the enviroment"
          , "variables APP_NAME, APP_ID and APP_SECRET, respectively.  "
          , "For example, before running the test you could run in the shell:"
          , ""
          , "  $ export APP_NAME=\"example\""
          , "  $ export APP_ID=\"458798571203498\""
          , "  $ export APP_SECRET=\"28a9d0fa4272a14a9287f423f90a48f2304\""
          , ""
          , "Of course, these values above aren't valid and you need to"
          , "replace them with your own."
          , ""
          , "(Exiting now with a failure code.)"]
        exitFailure

makeConf token = do
  expires <- addUTCTime 400000000 <$> getCurrentTime
  manager <- newManager tlsManagerSettings
  let userAT = Fb.UserAccessToken "10155182179270463" token expires
  creds <- getCredentials
  return (manager, userAT, creds)

main :: IO ()
main = do
  testCfg <- load [Required "tests.cfg"]
  (Just token) <- C.lookup testCfg "test.token"
  conf <- makeConf token
  apiTestTree <- apiTests conf
  --fbTestTree <- facebookSpecTests
  putStrLn "Starting tests"
  defaultMain (testGroup "Test Suite" [apiTestTree])


instance Configured (Maybe Text) where
  convert (String v) = case first of
    "Just" -> Just (Just second)
    _  -> Just Nothing
    where (first : second : _) = T.words v
  convert _ = Nothing

instance Configured IDType where
  convert (String v) = Just (Fb.Id v)
  convert _ = Nothing
