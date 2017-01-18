{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

module Howl.App.Users
  (
    usersHandlers
  ) where

import           Control.Exception.Lifted
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Logger         (MonadLogger, logError, logInfo,
                                               runStderrLoggingT)
import           Control.Monad.Trans.Resource

import           Data.String.Conversions

import           Database.Esqueleto           (from, select, (^.))
import qualified Database.Esqueleto           as E
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite      as Sql

import           Network.HTTP.Conduit         (Manager, newManager,
                                               tlsManagerSettings)
import           Network.Wai
import           Network.Wai.Handler.Warp     as Warp

import qualified Howl.Facebook                as Fb

import           Servant

import           Data.Text                    hiding (foldl, map)

import           Howl.Api.Users
import           Howl.App.Common
import           Howl.Models
import           Howl.Monad
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
  :<|> getUsersIdBlockedH
  :<|> postUsersIdBlockedH
  :<|> deleteUsersIdBlockedIdH
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

getUsersIdBlockedH :: IDType -> Maybe Token -> HandlerT IO [User]
getUsersIdBlockedH s mToken = runQuery $ do
  checkExistsOrThrow s
  userEntities <- E.select
        $ E.from
        $ \(user `E.InnerJoin` follow) -> do
        E.on (user^.UserFbID E.==. follow^.FollowshipTargetId
              E.&&. follow^.FollowshipSourceId E.==. E.val s)
        E.where_ (follow^.FollowshipStatus E.==. E.val Blocked)
        return user
  return $ map entityVal userEntities

postUsersIdBlockedH :: IDType -> IDType -> Maybe Token -> HandlerT IO IDType
postUsersIdBlockedH s t mToken =
  if s == t then throwError err409 else runQuery $ do
    checkExistsOrThrow s
    checkExistsOrThrow t
    getBy (UniqueFollowshipID s t) >>= \case
      Just (Entity _ (Followship _ _ Blocked)) -> throwError err409
      _ -> do
        deleteBy (UniqueFollowshipID t s)
        deleteBy (UniqueFollowshipID s t)
        insert (Followship s t Blocked)
        return t

deleteUsersIdBlockedIdH :: IDType -> IDType -> Maybe Token -> HandlerT IO IDType
deleteUsersIdBlockedIdH s t mToken = runQuery $ do
  checkExistsOrThrow s
  checkExistsOrThrow t
  getBy (UniqueFollowshipID s t) >>= \case
    Just (Entity _ (Followship _ _ Blocked)) -> deleteBy (UniqueFollowshipID s t) >> return t
    _ -> throwError err404


getUsersIdEventsFollowsH :: IDType -> Maybe Token -> HandlerT IO [Event]
getUsersIdEventsFollowsH i mToken = runQuery $ do
  checkExistsOrThrow i
  friends <- selectList [ FollowshipSourceId ==. i
                        , FollowshipStatus ==. Accepted] []
  let friendsIds = map (followshipTargetId . entityVal) friends
  let eventsConds = foldl (||.) [] $ map (\x -> [UserEventUserID ==. x]) friendsIds
  eventsUser <- selectList eventsConds []
  let eventsIds = map (userEventEventID . entityVal) eventsUser
  let conds = foldl (||.) [] $ map (\x -> [EventFbID ==. x]) eventsIds
  events <- selectList conds []
  return $ map entityVal events

getUsersIdEventsH :: IDType -> Maybe Token -> HandlerT IO [Event]
getUsersIdEventsH i mToken = runQuery $ do
  checkExistsOrThrow i
  eventsUser <- selectList [UserEventUserID ==. i] []
  let eventsIds = map (userEventEventID . entityVal) eventsUser
  let conds = foldl (||.) []  $ map (\x -> [EventFbID ==. x]) eventsIds
  events <- selectList conds []
  return $ map entityVal events
