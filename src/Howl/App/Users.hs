{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
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

import           Data.Text                hiding (map, foldl1)

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
  :<|> getUsersIdFollowsH
  :<|> postUsersIdFollowsH
  :<|> getUsersIdFollowsIdH
  :<|> deleteUsersIdFollowsIdH
  :<|> getUsersIdEventsFollowsH
  :<|> getUsersIdEventsH


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
    then throwError err403
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

getUsersIdFollowsH :: IDType -> Maybe Token -> HandlerT IO [User]
getUsersIdFollowsH i mToken =
  getUsersIdFollows i

getUsersIdFollows :: IDType -> HandlerT IO [User]
getUsersIdFollows i = runQuery $ do
  selectFirst [UserFbID ==. i] [] >>= \case
    Nothing -> throwError err404
    Just _ -> do
      userEntities <- E.select
        $ E.from
        $ \(user `E.InnerJoin` follow) -> do
        E.on (user^.UserFbID E.==. follow^.FollowshipTargetId
              E.&&. follow^.FollowshipSourceId E.==. E.val i)
        E.where_ (follow^.FollowshipStatus E.==. E.val Accepted)
        return user
      return $ map entityVal userEntities

-- userID -> content
postUsersIdFollowsH :: IDType -> IDType -> Maybe Token -> HandlerT IO IDType
postUsersIdFollowsH s t mToken =
  if s == t then throwError err409
  else runQuery $ do
    checkExistsOrThrow s
    checkExistsOrThrow t
    getBy (UniqueFollowshipID t s) >>= \case
      Just (Entity _ (Followship _ _ Blocked)) -> throwError err403
      _ -> insertBy (Followship s t Accepted) >>= \case
        Left _ -> throwError err409
        Right _ -> return t

deleteUsersIdFollowsIdH :: IDType -> IDType -> Maybe Token -> HandlerT IO IDType
deleteUsersIdFollowsIdH s t mToken = runQuery $ do
    checkExistsOrThrow s
    checkExistsOrThrow t
    getBy (UniqueFollowshipID s t) >>= \case
      Just _ -> deleteBy (UniqueFollowshipID s t) >> return t
      _ -> throwError err404


getUsersIdFollowsIdH :: IDType -> IDType -> Maybe Token -> HandlerT IO FollowStatus
getUsersIdFollowsIdH s t mToken = runQuery $ do
  checkExistsOrThrow s
  checkExistsOrThrow t
  getBy (UniqueFollowshipID s t) >>= \case
    Just (Entity _ (Followship _ _ status)) -> return status
    Nothing -> throwError err404

getUsersIdEventsFollowsH :: IDType -> Maybe Token -> HandlerT IO [Event]
getUsersIdEventsFollowsH i mToken = runQuery $ do
  checkExistsOrThrow i
  friends <- selectList [ FollowshipSourceId ==. i
                        , FollowshipStatus ==. Accepted] []
  let friendsIds = map (followshipTargetId . entityVal) friends
  let eventsConds = foldl1 (||.) $ map (\x -> [UserEventUserID ==. x]) friendsIds
  eventsUser <- selectList eventsConds []
  let eventsIds = map (userEventEventID . entityVal) eventsUser
  let conds = foldl1 (||.) $ map (\x -> [EventFbID ==. x]) eventsIds
  events <- selectList conds []
  return $ map entityVal events

getUsersIdEventsH :: IDType -> Maybe Token -> HandlerT IO [Event]
getUsersIdEventsH i mToken = runQuery $ do
  checkExistsOrThrow i
  eventsUser <- selectList [UserEventUserID ==. i] []
  let eventsIds = map (userEventEventID . entityVal) eventsUser
  let conds = foldl1 (||.) $ map (\x -> [EventFbID ==. x]) eventsIds
  events <- selectList conds []
  return $ map entityVal events

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

checkExistsOrThrow i = do
  mUser <- getBy $ UniqueUserID i
  case mUser of
    Nothing -> throwError err404
    Just (Entity k u) -> return u
