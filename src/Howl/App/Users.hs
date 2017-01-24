{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Howl.App.Users
  (
    usersHandlers
  ) where

import Prelude as P
import           Control.Exception.Lifted
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Logger         (MonadLogger, logError, logInfo, logDebug,
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

import           Data.Maybe
import           Data.Monoid                  ((<>))
import           Data.Text                    hiding (foldl, map)
import           Data.Text.Lazy               (fromStrict)
import           Data.Text.Lazy.Encoding      (encodeUtf8)

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
  :<|> getUsersIdEventsH
  :<|> getUsersIdEventsFollowsH
  :<|> getUsersIdVenuesH


getUsersH :: Maybe Token -> HandlerT IO [User]
getUsersH mToken = do
  $logInfo "Request: get all users"
  entities <- runQuery $ (select . from $ pure)
  return $ map entityVal entities

postUsersH :: Fb.UserAccessToken -> HandlerT IO User
postUsersH userAT = do
  $logInfo $ "Request: post new user"
  mResult <- postUsers userAT
  case mResult of
    Right u -> return u
    Left uid -> throwError err409{errBody = encodeUtf8 . fromStrict $ Fb.idCode uid}

postUsers :: Fb.UserAccessToken -> HandlerT IO (Either IDType User)
postUsers userAT = do
  creds' <- asks creds
  manager' <- asks manager
  runQuery $ do
    exists <- selectFirst [UserFbID ==. accessTokenUserId userAT] []
    case exists of
      Nothing -> Right <$> do
        u <- liftIO $ runResourceT $ getNewFbUser userAT creds' manager'
        $logDebug $ "Got user from facebook: " <> (pack . show) u

        es <- liftIO $ runResourceT $ getFbEvents userAT creds' manager' 100
        $logDebug $ "Got at least this 5 events from facebook: " <> (pack . show . P.take 5) es
        insert u
        mapM_ insertUnique es
        return u
      Just entity -> return (Left (userFbID $ entityVal entity))

putUsersH :: User -> Maybe Token -> HandlerT IO User
putUsersH u mToken = do
  $logInfo $ "Request putting user: " <> (pack . show) u
  runQuery $ do
    mUser <- getBy $ UniqueUserID (userFbID u)
    case mUser of
      Nothing -> do
        insert u
        return u
      Just (Entity k _) -> do
        Sql.replace k u --TODO Check username uniqueness
        return u

getUsersIdH :: IDType -> Maybe Token -> HandlerT IO User
getUsersIdH i mToken = do
  $logInfo $ "Request get user by id: " <> (pack . show) i
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
  $logInfo $ "Request put user by id: " <> (pack . show) i
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
  $logInfo $ "Request delete user by id: " <> (pack . show) i
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
getUsersIdFollowsH i mToken = do
  $logInfo $ "Request followed by user with id: " <> (pack . show) i
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
postUsersIdFollowsH s t mToken = do
  $logInfo $ "Request post follow: " <> (pack . show) s <> " to " <> (pack . show) t
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
deleteUsersIdFollowsIdH s t mToken = do
  $logInfo $ "Request delete follow: " <> (pack . show) s <> " to " <> (pack . show) t
  runQuery $ do
    checkExistsOrThrow s
    checkExistsOrThrow t
    getBy (UniqueFollowshipID s t) >>= \case
      Just _ -> deleteBy (UniqueFollowshipID s t) >> return t
      _ -> throwError err404


getUsersIdFollowsIdH :: IDType -> IDType -> Maybe Token -> HandlerT IO FollowStatus
getUsersIdFollowsIdH s t mToken = do
  $logInfo $ "Request get followship: " <> (pack . show) s <> " to " <> (pack . show) t
  runQuery $ do
    checkExistsOrThrow s
    checkExistsOrThrow t
    getBy (UniqueFollowshipID s t) >>= \case
      Just (Entity _ (Followship _ _ status)) -> return status
      Nothing -> throwError err404

getUsersIdBlockedH :: IDType -> Maybe Token -> HandlerT IO [User]
getUsersIdBlockedH s mToken = do
  $logInfo $ "Request user blocked list: " <> (pack . show) s
  runQuery $ do
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
postUsersIdBlockedH s t mToken = do
  $logInfo $ "Request block user: " <> (pack . show) s <> " to " <> (pack . show) t
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
deleteUsersIdBlockedIdH s t mToken = do
  $logInfo $ "Request delete block: " <> (pack . show) s <> " to " <> (pack . show) t
  runQuery $ do
    checkExistsOrThrow s
    checkExistsOrThrow t
    getBy (UniqueFollowshipID s t) >>= \case
      Just (Entity _ (Followship _ _ Blocked)) -> deleteBy (UniqueFollowshipID s t) >> return t
      _ -> throwError err404

getUsersIdEventsFollowsH :: IDType -> Maybe Token -> HandlerT IO [Event]
getUsersIdEventsFollowsH i mToken = do
  $logInfo $ "Request events of followed by user with id: " <> (pack . show) i
  runQuery $ do
    checkExistsOrThrow i
    eventEntities <- E.select $ E.distinct
      $ E.from
      $ \(event `E.InnerJoin` rsvp `E.InnerJoin` follows) -> do
      E.on (rsvp^.EventRSVPUserID E.==. follows^.FollowshipTargetId
         E.&&. follows^.FollowshipSourceId E.==. E.val i
         E.&&. follows^.FollowshipStatus E.==. E.val Accepted)
      E.on (event^.EventFbID E.==. rsvp^.EventRSVPEventID
         E.&&. rsvp^.EventRSVPRsvp E.!=. E.val Fb.Declined)
      return event
    return $ map entityVal eventEntities


getUsersIdEventsH :: IDType -> Maybe Token -> HandlerT IO [Event]
getUsersIdEventsH i mToken = do
  $logInfo $ "Request get events of user with id: " <> (pack . show) i
  runQuery $ do
    checkExistsOrThrow i
    eventEntities <- E.select
      $ E.from
      $ \(event `E.InnerJoin` rsvp) -> do
      E.on (event^.EventFbID E.==. rsvp^.EventRSVPEventID
         E.&&. rsvp^.EventRSVPUserID E.==. E.val i
         E.&&. rsvp^.EventRSVPRsvp E.!=. E.val Fb.Declined)
      return event
    return $ map entityVal eventEntities

getUsersIdVenuesH :: IDType -> Maybe Token -> HandlerT IO [Venue]
getUsersIdVenuesH ui mToken = do
  $logInfo $ "Request venues of user with id: " <> (pack . show) ui
  ui' <- tokenUser mToken
  runQuery $ do
    checkExistsOrThrow ui
    checkExistsOrThrowError ui' err401
    when (ui' /= ui) (throwError err403)
    eventEntities <- E.select
      $ E.from
      $ \(venue `E.InnerJoin` follower) -> do
      E.on (venue^.VenueFbID E.==. follower^.VenueFollowerVenueID
            E.&&. follower^.VenueFollowerUserID E.==. E.val ui)
      return venue
    return $ map entityVal eventEntities

--getNewUser :: (MonadBaseControl IO m, MonadResource m) =>  Fb.UserAccessToken -> Fb.Credentials -> Manager -> m User
getNewFbUser userAT creds manager =  do
  fbUser <- Fb.runFacebookT creds manager $ Fb.getUser (accessTokenUserId userAT) [("fields", "id,name,email,first_name,last_name")] (Just userAT)
  let
    fbID = Fb.userId fbUser
    username = fromMaybe (Fb.idCode fbID) (Fb.userUsername fbUser)
    firstName = fromMaybe username (Fb.userFirstName fbUser)
    lastName = Fb.userLastName fbUser
    email = Fb.userEmail fbUser
    profilePicPath = Nothing
    user = User fbID username firstName lastName email profilePicPath
  return user

getFbEvents :: (MonadBaseControl IO m, MonadResource m) =>  Fb.UserAccessToken -> Fb.Credentials -> Manager -> Int -> m [Event]
getFbEvents userAT creds manager limit = do
  let url = "/v2.8/" <> (Fb.idCode $ accessTokenUserId userAT) <> "/" <> "events"
  eventPager <- Fb.runFacebookT creds manager $ Fb.getObject url [("fields", "id,name,category,description,start_time,end_time,place,rsvp_status,owner")] (Just userAT)
  go eventPager []
  where go pager res =
          if P.length res < limit
          then do
            (Fb.runFacebookT creds manager $ Fb.fetchNextPage pager) >>= \case
              Just nextPager -> go nextPager (res ++ (Fb.pagerData pager))
              Nothing -> return res
          else return res
