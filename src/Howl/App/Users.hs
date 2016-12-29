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

import           Howl.Api
import           Howl.Models
import           Howl.Utils
import Howl.Types

usersHandlers :: ConnectionPool -> Manager -> Fb.Credentials -> Server Api
usersHandlers pool manager fbCredentials =
  postUsersH
  :<|> getUsersIdH
  :<|> putUsersIdH
  :<|> deleteUsersIdH
  :<|> getUsersIdFriendsH
  :<|> postUsersIdFriendsH
  :<|> deleteUsersIdFriendsIdH
  :<|> getUsersEventsH
  -- where
  --   postUserH userAT = do
  --     mResult <- liftIO $ postUser pool manager fbCredentials userAT
  --     case mResult of
  --       Just u -> return u
  --       Nothing -> throwError err409
  --   getUserIdH userID userAT = do
  --     mResult <- liftIO $ getUserId pool userID userAT
  --     case mResult of
  --       Just u -> return u
  --       Nothing -> throwError err404
  --   putUserIdH = undefined


--postUsersH :: Fb.UserAccessToken -> Server User
postUsersH = undefined
--getUsersIdH :: IDType -> Maybe Token -> Server User
getUsersIdH = undefined
--putUsersIdH :: IDType -> User -> Maybe Token -> Server User
putUsersIdH = undefined
--deleteUsersIdH :: IDType -> Maybe Token -> Server ()
deleteUsersIdH = undefined
--getUsersIdFriendsH :: IDType -> Maybe Token -> Server [User]
getUsersIdFriendsH = undefined
--postUsersIdFriendsH :: IDType -> IDType -> Maybe Token -> Server IDType
postUsersIdFriendsH = undefined
--deleteUsersIdFriendsIdH :: IDType -> IDType -> Maybe Token -> Server ()
deleteUsersIdFriendsIdH = undefined
--getUsersEventsH :: IDType -> Maybe Token -> Server [Event]
getUsersEventsH = undefined


postUser :: ConnectionPool -> Manager -> Fb.Credentials -> Fb.UserAccessToken -> IO (Maybe User)
postUser pool manager creds userAT = flip liftSqlPersistMPool pool $ do
  exists <- selectFirst [UserFbID ==. (accessTokenUserId userAT)] []
  case exists of
    Nothing -> Just <$> (do
                           u <- (getNewUser userAT creds manager)
                           insert u
                           return u)
    Just _ -> return Nothing

getUserId :: ConnectionPool -> IDType -> Fb.UserAccessToken -> IO (Maybe User)
getUserId pool userID userAT = flip runSqlPersistMPool pool $ do
  mUser <- selectFirst [UserFbID ==. userID] []
  return $ entityVal <$> mUser
