{-# LANGUAGE FlexibleContexts #-}

module App where

import           Control.Monad.IO.Class
import           Control.Monad.Logger (runStderrLoggingT)

import           Data.String.Conversions

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Network.Wai
import           Network.Wai.Handler.Warp as Warp
import Network.HTTP.Conduit (Manager, newManager, tlsManagerSettings)

import qualified Facebook as Fb

import           Servant

import           Data.Text

import           Api
import           Models
import           Utils

server :: ConnectionPool -> Manager -> Fb.Credentials -> Server Api
server pool manager fbCredentials =
  postUserH :<|> getUserGetH
  where
    postUserH userAT = do
      mResult <- liftIO $ postUser pool manager fbCredentials userAT
      case mResult of
        Just k -> return $ Just k
        Nothing -> throwError err409
    getUserGetH userID = liftIO $ getUserGet pool userID

postUser :: ConnectionPool -> Manager -> Fb.Credentials -> Fb.UserAccessToken -> IO (Maybe (Key User))
postUser pool manager creds userAT = flip liftSqlPersistMPool pool $ do
  exists <- selectFirst [UserFbID ==. (accessTokenUserId userAT)] []
  case exists of
    Nothing -> Just <$> (insert =<< (getNewUser userAT creds manager))
    Just _ -> return Nothing

getUserGet :: ConnectionPool -> IDType -> IO (Maybe User)
getUserGet pool userID = flip runSqlPersistMPool pool $ do
  mUser <- selectFirst [UserFbID ==. userID] []
  return $ entityVal <$> mUser

app :: ConnectionPool -> Manager -> Fb.Credentials -> Application
app pool manager fbCredentials = serve api $ server pool manager fbCredentials

sizeOfSqlitePool = 10

mkApp :: FilePath -> Fb.Credentials -> IO Application
mkApp sqliteFile fbCredentials = do
  pool <- runStderrLoggingT $ do
    createSqlitePool (cs sqliteFile) sizeOfSqlitePool

  runSqlPool (runMigration migrateAll) pool
  manager <- newManager tlsManagerSettings
  return $ app pool manager fbCredentials

run sqlFile fbCredentials = Warp.run 3000 =<< mkApp sqlFile fbCredentials
