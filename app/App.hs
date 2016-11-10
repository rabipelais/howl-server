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

import           Servant

import           Data.Text

import           Api
import           Models

server :: ConnectionPool -> Server Api
server pool =
  postUserH :<|> getUserGetH
  where
    postUserH newUser = do
      mResult <- liftIO $ postUser pool newUser
      case mResult of
        Just k -> return $ Just k
        Nothing -> throwError err409
    getUserGetH userID = liftIO $ getUserGet pool userID

postUser :: ConnectionPool -> User -> IO (Maybe (Key User))
postUser pool newUser = flip liftSqlPersistMPool pool $ do
  exists <- selectFirst [UserFbID ==. (userFbID newUser)] []
  case exists of
    Nothing -> Just <$> insert newUser
    Just _ -> return Nothing

getUserGet :: ConnectionPool -> IDType -> IO (Maybe User)
getUserGet pool userID = flip runSqlPersistMPool pool $ do
  mUser <- selectFirst [UserFbID ==. userID] []
  return $ entityVal <$> mUser

app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ do
    createSqlitePool (cs sqliteFile) 5

  runSqlPool (runMigration migrateAll) pool
  return $ app pool

run sqlFile = Warp.run 3000 =<< mkApp sqlFile
