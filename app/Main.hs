{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Configurator        as C
import           Howl
import qualified Howl.Facebook            as Fb
import qualified Howl.Logger              as Logger

import           Control.Monad.IO.Class
import           Control.Monad.Logger

import           Data.String.Conversions

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Network.HTTP.Conduit     (Manager, newManager,
                                           tlsManagerSettings)
import           Network.Wai
import           Network.Wai.Handler.Warp as Warp

--------------------------------------------------

getCredentials config = do
  appName <- C.lookup config "credentials.appName"
  appId <- C.lookup config "credentials.appId"
  appSecret <- C.lookup config "credentials.appSecret"
  return $ Fb.Credentials <$> appName <*> appId <*> appSecret

main :: IO ()
main = do
  let configPath = "app.cfg"
  config <- load [Required configPath]
  mCreds <- getCredentials config
  (Just (dbName :: String)) <- C.lookup config "database.dbName"
  (Just poolSize) <- C.lookup config "database.poolSize"
  let logSettings = Logger.Settings
        { Logger.filePath = "webserver.log"
        , Logger.logLevel = LevelDebug
        , Logger.noConsoleLogging = False
        }
  putStrLn "Starting server"
  case mCreds of
    Nothing -> print "ERROR reading config file"
    Just creds -> Logger.withLogger logSettings $ \logFn -> do
      let port = 3000 :: Int
      liftIO $ putStrLn $ "Listening on port " ++ show port ++ " ..."
      pool <- runStderrLoggingT $ createSqlitePool (cs dbName) poolSize
      runSqlPool (runMigration migrateAll) pool
      manager <- liftIO $ newManager tlsManagerSettings
      let env = LogEnv logFn $ HandlerEnv pool manager creds
      liftIO $ Warp.run port $ app pool manager creds
