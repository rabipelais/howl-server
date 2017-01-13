{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Howl.App.Users
  (
    usersHandlers
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger     (runStderrLoggingT, MonadLogger, logError, logInfo)
import Control.Monad.Except
import Control.Exception.Lifted
import Control.Monad.Trans.Resource

import           Data.String.Conversions

import           Database.Esqueleto       ((^.), select, from)
import qualified Database.Esqueleto       as E
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite  as Sql

import           Network.HTTP.Conduit     (Manager, newManager,
                                           tlsManagerSettings)
import           Network.Wai
import           Network.Wai.Handler.Warp as Warp

import qualified Howl.Facebook            as Fb

import           Servant

import           Data.Text                hiding (map)

import           Howl.Api.Users
import           Howl.Models
import           Howl.Types
import           Howl.Utils

usersHandlers :: ServerT UsersAPI (HandlerT IO)
usersHandlers =
  getUsersH
  :<|> postUsersH
  :<|> putUsersH
  :<|> getUsersIdH
  :<|> putUsersIdH
  :<|> deleteUsersIdH
  -- :<|> getUsersIdConnectH
  :<|> getUsersIdFollowingH
  -- :<|> postUsersIdFriendsH
  -- :<|> getUsersIdFriendsEventsH
  -- :<|> deleteUsersIdFriendsIdH
  -- :<|> getUsersIdEventsH


getUsersH :: Maybe Token -> HandlerT IO [User]
getUsersH mToken = do
  entities <- runQuery $ (select . from $ pure)
  return $ map entityVal entities

postUsersH :: Fb.UserAccessToken -> HandlerT IO User
postUsersH userAT = do
  mResult <- postUsers userAT
  case mResult of
    Just u -> return u
    Nothing -> throwError err409

postUsers :: Fb.UserAccessToken -> HandlerT IO (Maybe User)
postUsers userAT = do
  creds' <- asks creds
  manager' <- asks manager
  u <- liftIO $ runResourceT $ getNewUser userAT creds' manager'
  runQuery $ do
    exists <- selectFirst [UserFbID ==. accessTokenUserId userAT] []
    case exists of
      Nothing -> Just <$> (do
                           insert u
                           return u)
      Just _ -> return Nothing

getUsersIdH :: IDType -> Maybe Token -> HandlerT IO User
getUsersIdH i mToken = do
  mResult <- getUsersId i
  case mResult of
    Just u -> return u
    Nothing -> throwError err404

getUsersId :: IDType -> HandlerT IO (Maybe User)
getUsersId userID = runQuery $ do
  mUser <- selectFirst [UserFbID ==. userID] []
  return $ entityVal <$> mUser

putUsersIdH :: IDType -> User -> Maybe Token -> HandlerT IO User
putUsersIdH i u mToken = do
  if userFbID u /= i
    then liftIO (print "id doesn't match") >> throwError err400
    else do
      eRep <- putUserId i u
      case eRep of
        Left e -> throwError e
        Right u -> return u

putUserId :: IDType -> User -> HandlerT IO (Either ServantErr User)
putUserId i u = runQuery $ do
  mUser <- getBy $ UniqueUserID i
  case mUser of
    Nothing -> return $ Left err404
    Just (Entity k _) -> do
      Sql.replace k u --TODO Check username uniqueness
      return $ Right u

putUsersH :: User -> Maybe Token -> HandlerT IO User
putUsersH u mToken =  runQuery $ do
  mUser <- getBy $ UniqueUserID (userFbID u)
  case mUser of
    Nothing -> do
      insert u
      return u
    Just (Entity k _) -> do
      Sql.replace k u --TODO Check username uniqueness
      return u

deleteUsersIdH :: IDType -> Maybe Token -> HandlerT IO IDType
deleteUsersIdH i mToken = do
  eRep <- deleteUserId i
  case eRep of
    Left e -> throwError e
    Right _ -> return i

deleteUserId i = runQuery $ do
  mUser <- getBy $ UniqueUserID i
  case mUser of
    Nothing -> return $ Left err404
    Just (Entity k u) -> do
      Sql.delete k
      return $ Right i

getUsersIdConnectH = undefined

getUsersIdFollowingH :: IDType -> Maybe Token -> HandlerT IO [User]
getUsersIdFollowingH i mToken = do
  getUsersIdFollowing i

getUsersIdFollowing :: IDType -> HandlerT IO [User]
getUsersIdFollowing i = runQuery $ do
  userEntities <- E.select
    $ E.from
    $ \(user `E.InnerJoin` follow) -> do
    E.on (user^.UserFbID E.==. follow^.FollowshipTargetId
          E.&&. follow^.FollowshipSourceId E.==. E.val i)
    E.where_ (follow^.FollowshipStatus E.==. E.val Accepted)
    return user
  return $ map entityVal userEntities


--postUsersIdFriendsH :: IDType -> IDType -> Maybe Token -> Server IDType
postUsersIdFriendsH = undefined
--deleteUsersIdFriendsIdH :: IDType -> IDType -> Maybe Token -> Server ()
deleteUsersIdFriendsIdH = undefined

getUsersIdFriendsEventsH = undefined
--getUsersEventsH :: IDType -> Maybe Token -> Server [Event]
getUsersIdEventsH = undefined

runDb :: (MonadLogger (HandlerT IO), MonadError e (HandlerT IO))
      => ConnectionPool -> e -> SqlPersistT (HandlerT IO) a -> HandlerT IO a
runDb pool err q =
  catch (runSqlPool q pool) $ \(SomeException e) -> do
    $logError "runSqlPool failed."
    $logError $ "Error: " <> (pack . show) e
    throwError err

runQuery :: SqlPersistT (HandlerT IO) a -> HandlerT IO a
runQuery query = do
  pool <- asks db
  runDb pool err500 query
