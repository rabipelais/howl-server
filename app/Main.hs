{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Configurator           as C
import           Howl
import qualified Howl.Facebook               as Fb
import qualified Howl.Logger                 as Logger

import           Control.Monad.IO.Class
import           Control.Monad.Logger

import           Data.String.Conversions

import           Data.ByteString.Char8       (pack)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.Sql

import           Network.HTTP.Conduit        (Manager, newManager,
                                              tlsManagerSettings)
import           Network.Wai
import           Network.Wai.Handler.Warp    as Warp

--------------------------------------------------

getCredentials config = do
  appName <- C.lookup config "credentials.appName"
  appId <- C.lookup config "credentials.appId"
  appSecret <- C.lookup config "credentials.appSecret"
  return $ Fb.Credentials <$> appName <*> appId <*> appSecret

main :: IO ()
main = do
  let configPath = "app.cfg"
  putStrLn "Reading config..."
  config <- load [Required configPath]
  mCreds <- getCredentials config
  (Just (dbName :: String)) <- C.lookup config "database.name"
  (Just (dbHost :: String)) <- C.lookup config "database.host"
  (Just (dbUser :: String)) <- C.lookup config "database.user"
  (Just (dbPassword :: String)) <- C.lookup config "database.password"
  (Just (dbPort :: String)) <- C.lookup config "database.port"
  (Just poolSize) <- C.lookup config "database.poolSize"
  let logSettings = Logger.Settings
        { Logger.filePath = "webserver.log"
        , Logger.logLevel = LevelDebug
        , Logger.noConsoleLogging = False
        }
      connString = pack $ "host=" <> dbHost <> " dbname=" <> dbName <> " user=" <> dbUser <> " password=" <> dbPassword <> " port=" <> dbPort
  putStrLn "Starting server"
  case mCreds of
    Nothing -> print "ERROR reading config file"
    Just creds -> Logger.withLogger logSettings $ \logFn -> do
      let port = 3000 :: Int
      liftIO $ putStrLn $ "Listening on port " ++ show port ++ " ..."
      runStderrLoggingT $ withPostgresqlPool connString poolSize $ \pool -> do
        runSqlPool (runMigration migrateAll) pool
        manager <- liftIO $ newManager tlsManagerSettings
        let env = LogEnv logFn $ authHandlerEnv pool manager creds
        liftIO $ Warp.run port $ app env
