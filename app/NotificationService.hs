{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Text                   as T
import           Howl
import qualified Howl.Facebook               as Fb
import qualified Howl.Logger                 as Logger
import           Howl.Notifications
import qualified Howl.Queue                  as Q

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

main :: IO ()
main = do
  putStrLn "Reading config..."
  (dbName, dbHost, dbUser, dbPassword, dbPort, poolSize, mqHost, mqVHost, mqUser, mqPassword) <- getConfig
  let logSettings = Logger.Settings
        { Logger.filePath = "/var/log/webserver.log"
        , Logger.logLevel = LevelDebug
        , Logger.noConsoleLogging = False
        }
      connString = pack $ "host=" <> dbHost <> " dbname=" <> dbName <> " user=" <> dbUser <> " password=" <> dbPassword <> " port=" <> dbPort
      queueSetting = Q.Settings mqHost mqVHost mqUser mqPassword
      queues = [Q.newQueue{Q.queueName = "notifications_task", Q.queueAutoDelete = False, Q.queueDurable = True}]
  putStrLn "Starting notification service"
  sendNotification
  -- Logger.withLogger logSettings $ \logFn -> do
  --   liftIO $ Q.withConnection queueSetting queues $ \(conn, chan) -> do
  --     let port = 3000 :: Int
  --     liftIO $ putStrLn $ "Listening on port " ++ show port ++ " ..."
  --     runStderrLoggingT $ withPostgresqlPool connString poolSize $ \pool -> do
  --       sendNotification
  --       --runSqlPool (runMigration migrateAll) pool
  --       --manager <- liftIO $ newManager tlsManagerSettings
  --       --let env = LogEnv logFn $ authHandlerEnv pool manager creds chan
        --liftIO $ Warp.run port $ app env

getConfig :: IO (String, String, String, String, String, Int, String, T.Text, T.Text, T.Text)
getConfig = tryToGet `E.catch` showHelp
  where
    tryToGet = do
      [dbName, dbHost, dbUser, dbPassword, dbPort, poolsize, mqHost, mqVHost, mqUser, mqPassword] <- mapM getEnv ["DB_NAME", "DB_HOST", "DB_USER", "DB_PASSWORD", "DB_PORT", "DB_POOLSIZE", "AMQP_HOST", "AMQP_VHOST", "AMQP_USER", "AMQP_PASSWORD"]
      return (dbName, dbHost, dbUser, dbPassword, dbPort, read poolsize, mqHost, T.pack mqVHost, T.pack mqUser, T.pack mqPassword)
    showHelp exc | not (isDoesNotExistError exc) = E.throw exc
    showHelp _ = do
      putStrLn $ unlines
          [ "In order to run the notifications service, you need"
          , "the following configuration environment variables:"
          , ""
          , "AMQP_HOST             RabbitMQ Host"
          , "AMQP_VHOST            RabbitMQ Virtual Host"
          , "AMQP_USER             RabbitMQ User"
          , "AMQP_PASSWORD         RabbitMQ password"
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
