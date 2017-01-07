module Howl.App.Users where

import           Control.Monad.IO.Class
import           Control.Monad.Logger (runStderrLoggingT)

import           Data.String.Conversions

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite as Sql

import           Network.Wai
import           Network.Wai.Handler.Warp as Warp
import Network.HTTP.Conduit (Manager, newManager, tlsManagerSettings)

import qualified Howl.Facebook as Fb

import           Servant

import           Data.Text hiding (map)

import           Howl.Api.Users
import           Howl.Models
import           Howl.Utils
import Howl.Types

type Resources = (ConnectionPool, Manager, Fb.Credentials)

usersHandlers :: Resources -> Server UsersAPI
usersHandlers s@(p, m, c) =
  (getUsersH s)
  :<|> (postUsersH s)
  :<|> (getUsersIdH s)
  :<|> (putUsersIdH s)
  :<|> (deleteUsersIdH s)
  -- :<|> getUsersIdConnectH
  -- :<|> getUsersIdFriendsH
  -- :<|> postUsersIdFriendsH
  -- :<|> getUsersIdFriendsEventsH
  -- :<|> deleteUsersIdFriendsIdH
  -- :<|> getUsersIdEventsH


getUsersH :: Resources -> Maybe Token -> Handler [User]
getUsersH (p, m, c) mToken = do
  liftIO $ print "GET Users"
  liftIO $ flip liftSqlPersistMPool p $ do
    userEntities <- selectList [] []
    return $ map (\(Entity _ u) -> u) userEntities

postUsersH :: Resources -> Fb.UserAccessToken -> Handler User
postUsersH (pool, manager, fbCredentials) userAT = do
  mResult <- liftIO $ postUsers pool manager fbCredentials userAT
  case mResult of
    Just u -> return u
    Nothing -> throwError err409

postUsers :: ConnectionPool -> Manager -> Fb.Credentials -> Fb.UserAccessToken -> IO (Maybe User)
postUsers pool manager creds userAT = flip liftSqlPersistMPool pool $ do
  exists <- selectFirst [UserFbID ==. (accessTokenUserId userAT)] []
  case exists of
    Nothing -> Just <$> (do
                           u <- (getNewUser userAT creds manager)
                           insert u
                           return u)
    Just _ -> return Nothing

getUsersIdH :: Resources -> IDType -> Maybe Token -> Handler User
getUsersIdH (p, m, c) i mToken = do
  mResult <- liftIO $ getUsersId p i
  case mResult of
    Just u -> return u
    Nothing -> throwError err404

getUsersId :: ConnectionPool -> IDType -> IO (Maybe User)
getUsersId pool userID = flip runSqlPersistMPool pool $ do
  mUser <- selectFirst [UserFbID ==. userID] []
  return $ entityVal <$> mUser

putUsersIdH :: Resources -> IDType -> User -> Maybe Token -> Handler User
putUsersIdH (p, m, c) i u mToken = do
  liftIO $ print "PUT {userID}"
  if (userFbID u /= i)
    then (liftIO $ print "id doesn't match") >> throwError err400
    else do
      liftIO $ print "Fb ID and User dataload match"
      eRep <- liftIO $ putUserId p i u
      case eRep of
        Left e -> throwError e
        Right u -> return u

putUserId :: ConnectionPool -> IDType -> User -> IO (Either ServantErr User)
putUserId pool i u = flip runSqlPersistMPool pool $ do
  mUser <- getBy $ UniqueUserID i
  case mUser of
    Nothing -> return $ Left err404
    Just (Entity k _) -> do
      liftIO $ print "User found"
      Sql.replace k u --TODO Check username uniqueness
      return $ Right u

deleteUsersIdH :: Resources -> IDType -> Maybe Token -> Handler IDType
deleteUsersIdH (p, m, c) i mToken = do
  eRep <- liftIO $ deleteUserId p i
  case eRep of
    Left e -> throwError e
    Right _ -> return i

deleteUserId pool i = flip runSqlPersistMPool pool $ do
  mUser <- getBy $ UniqueUserID i
  case mUser of
    Nothing -> return $ Left err404
    Just (Entity k u) -> do
      Sql.delete k
      return $ Right u

getUsersIdConnectH = undefined

--getUsersIdFriendsH :: IDType -> Maybe Token -> Server [User]
getUsersIdFriendsH = undefined
--postUsersIdFriendsH :: IDType -> IDType -> Maybe Token -> Server IDType
postUsersIdFriendsH = undefined
--deleteUsersIdFriendsIdH :: IDType -> IDType -> Maybe Token -> Server ()
deleteUsersIdFriendsIdH = undefined

getUsersIdFriendsEventsH = undefined
--getUsersEventsH :: IDType -> Maybe Token -> Server [Event]
getUsersIdEventsH = undefined
