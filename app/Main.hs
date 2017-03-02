{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Text                   as T
import           Howl
import qualified Howl.Facebook               as Fb
import qualified Howl.Logger                 as Logger

import qualified Control.Exception.Lifted    as E
import           Control.Monad.IO.Class
import           Control.Monad.Logger

import           Data.String.Conversions

import           Data.ByteString.Char8       (pack)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.Sql

import           System.Environment          (getEnv)
import           System.Exit                 (exitFailure)
import           System.IO.Error             (isDoesNotExistError)

import           Network.HTTP.Conduit        (Manager, newManager,
                                              tlsManagerSettings)
import           Network.Wai
import           Network.Wai.Handler.Warp    as Warp

--------------------------------------------------

main :: IO ()
main = do
  putStrLn "Reading config..."
  (dbName, dbHost, dbUser, dbPassword, dbPort, poolSize, mqHost, mqVHost, mqUser, mqPassword, creds) <- getConfig
  let logSettings = Logger.Settings
        { Logger.filePath = "/var/log/webserver.log"
        , Logger.logLevel = LevelDebug
        , Logger.noConsoleLogging = False
        }
      connString = pack $ "host=" <> dbHost <> " dbname=" <> dbName <> " user=" <> dbUser <> " password=" <> dbPassword <> " port=" <> dbPort
  putStrLn "Starting server"
  Logger.withLogger logSettings $ \logFn -> do
      let port = 3000 :: Int
      liftIO $ putStrLn $ "Listening on port " ++ show port ++ " ..."
      runStderrLoggingT $ withPostgresqlPool connString poolSize $ \pool -> do
        runSqlPool (runMigration migrateAll) pool
        manager <- liftIO $ newManager tlsManagerSettings
        let env = LogEnv logFn $ authHandlerEnv pool manager creds
        liftIO $ Warp.run port $ app env

getConfig = tryToGet `E.catch` showHelp
  where
    tryToGet = do
      [dbName, dbHost, dbUser, dbPassword, dbPort, poolsize, appName, appId, appSecret, mqHost, mqVHost, mqUser, mqPassword] <- mapM getEnv ["DB_NAME", "DB_HOST", "DB_USER", "DB_PASSWORD", "DB_PORT", "DB_POOLSIZE", "APP_NAME", "APP_ID", "APP_SECRET", "AMPQ_HOST", "AMPQ_VHOST", "AMPQ_USER", "AMPQ_PASSWORD"]
      return (dbName, dbHost, dbUser, dbPassword, dbPort, read poolsize, mqHost, mqVHost, mqUser, mqPassword, Fb.Credentials (T.pack appName) (T.pack appId)  (T.pack appSecret))
    showHelp exc | not (isDoesNotExistError exc) = E.throw exc
    showHelp _ = do
      putStrLn $ unlines
          [ "In order to run the webserver, you need"
          , "the following configuration environment variables:"
          , ""
          , "APP_SECRET            Facebook app secret key"
          , "APP_ID                Facebook app ID"
          , "APP_NAME              Facebook app name (Howl)"
          , "AWS_ACCESS_KEY_ID     AWS secret key ID"
          , "AWS_ACCOUNT_ID        AWS account ID"
          , "AWS_DEFAULT_REGION    AWS region (eu-central-1)"
          , "AWS_REPO_NAME         AWS docker repo (howl-docker-repo)"
          , "AWS_S3_LOCATION       AWS S3 bucket location"
          , "AWS_SECRET_ACCESS_KEY AWS secret key"
          , "AMPQ_HOST             RabbitMQ Host"
          , "AMPQ_VHOST            RabbitMQ Virtual Host"
          , "AMPQ_USER             RabbitMQ User"
          , "AMPQ_PASSWORD         RabbitMQ password"
          , "DB_NAME               DB name"
          , "DB_USER               DB user"
          , "DB_HOST               DB host"
          , "DB_PASSWORD           DB password"
          , "DB_PORT               DB port (5432)"
          , "DB_POOLSIZE           DB connection pool size"
          , ""
          , "For example, before running the webserver you could run in the shell:"
          , ""
          , "  $ export APP_NAME=\"example\""
          , "  $ export APP_ID=\"458798571203498\""
          , "  $ export APP_SECRET=\"28a9d0fa4272a14a9287f423f90a48f2304\""
          , "  ..."
          , ""
          , "or if you're using fish:"
          , ""
          , "  $ set -g -x APP_NAME \"example\""
          , "  $ set -g -x APP_ID \"458798571203498\""
          , "  $ set -g -x APP_SECRET \"28a9d0fa4272a14a9287f423f90a48f2304\""
          , "  ..."
          , ""
          , "Of course, the values above aren't valid and you need to"
          , "replace them with your own."
          , ""
          , "(Exiting now with a failure code.)"]
      exitFailure
