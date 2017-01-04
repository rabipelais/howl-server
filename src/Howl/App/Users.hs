module Howl.App.Users where

import           Control.Monad.IO.Class
import           Control.Monad.Logger (runStderrLoggingT)

import           Data.String.Conversions

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Network.Wai
import           Network.Wai.Handler.Warp as Warp
import Network.HTTP.Conduit (Manager, newManager, tlsManagerSettings)

import qualified Howl.Facebook as Fb

import           Servant

import           Data.Text

import           Howl.Api.Users
import           Howl.Models
import           Howl.Utils
import Howl.Types

type Resources = (ConnectionPool, Manager, Fb.Credentials)

usersHandlers :: Resources -> Server UsersAPI
usersHandlers s@(p, m, c) =
  (postUsersH s)
  :<|> (getUsersIdH s)
  -- :<|> putUsersIdH
  -- :<|> deleteUsersIdH
  -- :<|> getUsersIdConnectH
  -- :<|> getUsersIdFriendsH
  -- :<|> postUsersIdFriendsH
  -- :<|> getUsersIdFriendsEventsH
  -- :<|> deleteUsersIdFriendsIdH
  -- :<|> getUsersIdEventsH

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
getUsersIdH (p, m, c) i (Just t) = do
  mResult <- liftIO $ getUsersId p i
  case mResult of
    Just u -> return u
    Nothing -> throwError err404

getUsersId :: ConnectionPool -> IDType -> IO (Maybe User)
getUsersId pool userID = flip runSqlPersistMPool pool $ do
  mUser <- selectFirst [UserFbID ==. userID] []
  return $ entityVal <$> mUser

--putUsersIdH :: IDType -> User -> Maybe Token -> Server User
putUsersIdH = undefined

getUsersIdConnectH = undefined
--deleteUsersIdH :: IDType -> Maybe Token -> Server ()
deleteUsersIdH = undefined
--getUsersIdFriendsH :: IDType -> Maybe Token -> Server [User]
getUsersIdFriendsH = undefined
--postUsersIdFriendsH :: IDType -> IDType -> Maybe Token -> Server IDType
postUsersIdFriendsH = undefined
--deleteUsersIdFriendsIdH :: IDType -> IDType -> Maybe Token -> Server ()
deleteUsersIdFriendsIdH = undefined

getUsersIdFriendsEventsH = undefined
--getUsersEventsH :: IDType -> Maybe Token -> Server [Event]
getUsersIdEventsH = undefined
