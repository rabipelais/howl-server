{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Howl.App.Users
  (
    usersHandlers
  ) where

import Prelude as P
import Debug.Trace
import           Control.Exception.Lifted
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Logger         (MonadLogger, logError, logInfo, logDebug,
                                               runStderrLoggingT)
import           Control.Monad.Trans.Resource

import           Data.String.Conversions

import           Database.Esqueleto           (from, select, (^.), (?.))
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
import Data.Text as T                  hiding (foldl, map)
import           Data.Text.Lazy               (fromStrict)
import           Data.Text.Lazy.Encoding      (encodeUtf8)
import qualified Data.Time                    as TI
import           Data.Time.Clock
import Data.Time.Clock.POSIX

import           Howl.Api.Users
import           Howl.Api.Common as Api
import           Howl.App.Common
import           Howl.Models as Model
import           Howl.Monad
import           Howl.Types
import           Howl.Utils
import           Howl.Downloader
import           Howl.Notifications

usersHandlers :: ServerT UsersAPI (HandlerT IO)
usersHandlers =
  getUsersH
  :<|> postUsersH
  :<|> putUsersH
  :<|> getUsersIdH
  :<|> putUsersIdH
  :<|> deleteUsersIdH
  :<|> getUsersIdConnectH
  :<|> getUsersIdFollowsH
  :<|> getUsersIdFollowersH
  :<|> postUsersIdFollowsH
  :<|> getUsersIdFollowsIdH
  :<|> deleteUsersIdFollowsIdH
  :<|> getUsersIdFollowingCountH
  :<|> getUsersIdFollowersCountH
  :<|> getUsersIdBlockedH
  :<|> postUsersIdBlockedH
  :<|> deleteUsersIdBlockedIdH
  :<|> getUsersIdEventsH
  :<|> getUsersIdEventsFollowsH
  :<|> getUsersIdVenuesH
  :<|> getUsersIdSuggestedH
  :<|> getUsersIdDevicesH
  :<|> putUsersIdDevicesIdH
  :<|> deleteUsersIdDevicesIdH
  :<|> getUsersIdAgendaH
  :<|> getUsersIdPromotionsH


getUsersH :: Maybe Token -> HandlerT IO [User]
getUsersH mToken = do
  $logInfo "Request: get all users"
  entities <- runQuery $ (select . from $ pure)
  return $ P.map entityVal entities

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
        insert u
        $logDebug $ "Got user from facebook: " <> (pack . show) u

        ers <- liftIO $ runResourceT $ getFbEvents userAT creds' manager' 300
        mapM_ (insertUnique . fst) ers
        mapM_ (attendEvent u) ers
        $logDebug $ "Got user from facebook (events count): " <> (pack . show . P.length) ers
        $logDebug $ "Got user from facebook (events): " <> (pack . show) u
        let venueIds = P.filter (not . T.null) $ P.map (Fb.idCode .  eventVenueId . fst) ers
        vs <- mapM (liftIO . runResourceT . getFbVenue userAT creds' manager') venueIds
        $logDebug $ "Got user from facebook (venues): " <> (pack . show) u

        -- $logDebug $ "Got at least this 5 events from facebook: " <> (pack . show . P.take 5) es

        mapM_ insertUnique vs
        return u
      Just entity -> return (Left (userFbID $ entityVal entity))
  where
    attendEvent u (e, r) = do
      let rsvp = EventRSVP ui ei r
          ui = userFbID u
          ei = eventFbID e
      getBy (UniqueEventRSVP ui ei) >>= \case
        Just (Entity k _) -> Sql.replace k rsvp
        Nothing -> insert_ rsvp
      return rsvp

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

getUsersIdH :: IDType -> Maybe Token -> HandlerT IO ApiUser
getUsersIdH target mToken = do
  $logInfo $ "Request get user by id: " <> (pack . show) target
  source <- tokenUser mToken
  mResult <- getUsersId source target
  case mResult of
    Just u -> return u
    Nothing -> throwError err404

getUsersId :: IDType -> IDType -> HandlerT IO (Maybe ApiUser)
getUsersId source target = runQuery $ do
  res <- E.select
    $ E.from
    $ \(user `E.LeftOuterJoin` follow) -> do
    E.on (E.just (user^.UserFbID) E.==. follow?.FollowshipTargetId
          E.&&. follow?.FollowshipSourceId E.==. E.just (E.val source))
    E.where_ (user^.UserFbID E.==. E.val target)
    return (user, follow)
  let status f = fromMaybe None (followshipStatus . entityVal <$> f)
  return $ headMaybe $ map (\(u, f) -> toApiUser (Just (status f)) (entityVal u)) res
  where headMaybe [] = Nothing
        headMaybe (x:xs) = Just x

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

getUsersIdConnectH _ _ = throwError err405

getUsersIdFollowsH :: IDType -> Maybe Int -> Maybe Int -> Maybe Token -> HandlerT IO [ApiUser]
getUsersIdFollowsH i mLimit mOffset mToken = do
  $logInfo $ "Request followed by user with id: " <> (pack . show) i
  getUsersIdFollows i
  where
    l = maybe 10 fromIntegral mLimit
    o = maybe 0 fromIntegral mOffset

getUsersIdFollows :: IDType -> HandlerT IO [ApiUser]
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
      return $ map (toApiUser (Just Accepted) . entityVal) userEntities

getUsersIdFollowersH :: IDType -> Maybe Int -> Maybe Int -> Maybe Token -> HandlerT IO [ApiUser]
getUsersIdFollowersH i mLimit mOffset mToken = do
  $logInfo $ "Request followed by user with id: " <> (pack . show) i
  getUsersIdFollowers i
  where
    l = maybe 10 fromIntegral mLimit
    o = maybe 0 fromIntegral mOffset

getUsersIdFollowers :: IDType -> HandlerT IO [ApiUser]
getUsersIdFollowers i = runQuery $ do
  selectFirst [UserFbID ==. i] [] >>= \case
    Nothing -> throwError err404
    Just _ -> do
      userEntities <- E.select
        $ E.from
        $ \(user `E.InnerJoin` follow) -> do
        E.on (E.val i E.==. follow^.FollowshipTargetId
              E.&&. follow^.FollowshipSourceId E.==. user^.UserFbID)
        E.where_ (follow^.FollowshipStatus E.==. E.val Accepted)
        return user
      return $ map (toApiUser (Just Accepted) . entityVal) userEntities

postUsersIdFollowsH :: IDType -> IDType -> Maybe Token -> HandlerT IO IDType
postUsersIdFollowsH s t mToken = do
  $logInfo $ "Request post follow: " <> (pack . show) s <> " to " <> (pack . show) t
  ch <- asks queueChan
  if s == t then throwError err409
  else runQuery $ do
    checkExistsOrThrow s
    checkExistsOrThrow t
    getBy (UniqueFollowshipID t s) >>= \case
      Just (Entity _ (Followship _ _ Blocked)) -> throwError err403
      _ -> insertBy (Followship s t Accepted) >>= \case
        Left _ -> throwError err409
        Right _ -> liftIO $ sendFollowTask ch s t >> return t

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

selectCount q = do
  res <- select $ from $ (\x -> q x >> return E.countRows)
  return $ fromMaybe 0 . listToMaybe . fmap (\(E.Value v) -> v) $ res

getUsersIdFollowingCountH :: IDType -> Maybe Token -> HandlerT IO Int
getUsersIdFollowingCountH ui mToken = do
  $logInfo $ "Request get following count: " <> (pack . show) ui
  runQuery $ do
    checkExistsOrThrow ui
    selectCount $ \follow -> do
        E.where_ (E.val ui E.==. follow^.FollowshipSourceId
                  E.&&. follow^.FollowshipStatus E.==. E.val Accepted)

getUsersIdFollowersCountH :: IDType -> Maybe Token -> HandlerT IO Int
getUsersIdFollowersCountH ui mToken = do
  $logInfo $ "Request get follower count: " <> (pack . show) ui
  runQuery $ do
    checkExistsOrThrow ui
    selectCount $ \follow -> do
        E.where_ (E.val ui E.==. follow^.FollowshipTargetId
                  E.&&. follow^.FollowshipStatus E.==. E.val Accepted)

getUsersIdBlockedH :: IDType -> Maybe Int -> Maybe Int -> Maybe Token -> HandlerT IO [ApiUser]
getUsersIdBlockedH s mLimit mOffset mToken = do
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
    return $ map (toApiUser (Just Blocked) . entityVal) userEntities
  where
    l = maybe 10 fromIntegral mLimit
    o = maybe 0 fromIntegral mOffset

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

getUsersIdEventsFollowsH :: IDType -> Maybe Int -> Maybe Int -> Maybe Token -> HandlerT IO [Api.Event]
getUsersIdEventsFollowsH i mLimit mOffset mToken = do
  $logInfo $ "Request events of followed by user with id: " <> (pack . show) i
  runQuery $ do
    checkExistsOrThrow i
    eventEntities <- friendsEvents i
    let es = map entityVal eventEntities
    mapM (intoApiEvent i) es
  where
    l = maybe 10 fromIntegral mLimit
    o = maybe 0 fromIntegral mOffset

friendsEvents i =  E.select $ E.distinct
      $ E.from
      $ \(event `E.InnerJoin` rsvp `E.InnerJoin` follows) -> do
      E.on (rsvp^.EventRSVPUserID E.==. follows^.FollowshipTargetId
         E.&&. follows^.FollowshipSourceId E.==. E.val i
         E.&&. follows^.FollowshipStatus E.==. E.val Accepted)
      E.on (event^.EventFbID E.==. rsvp^.EventRSVPEventID
         E.&&. rsvp^.EventRSVPRsvp E.!=. E.val Fb.Declined)
      return event

getUsersIdEventsH :: IDType -> Maybe Int -> Maybe Int -> Maybe Token -> HandlerT IO [Api.Event]
getUsersIdEventsH i mLimit mOffset mToken = do
  $logInfo $ "Request get events of user with id: " <> (pack . show) i
  runQuery $ do
    checkExistsOrThrow i
    eventEntities <- usersEvents i
    let es = map entityVal eventEntities
    mapM (intoApiEvent i) es
  where
    l = maybe 10 fromIntegral mLimit
    o = maybe 0 fromIntegral mOffset

usersEvents i = E.select
      $ E.from
      $ \(event `E.InnerJoin` rsvp) -> do
      E.on (event^.EventFbID E.==. rsvp^.EventRSVPEventID
         E.&&. rsvp^.EventRSVPUserID E.==. E.val i
         E.&&. rsvp^.EventRSVPRsvp E.!=. E.val Fb.Declined)
      E.orderBy [E.desc (event^.EventStartTime)]
      return event

getUsersIdVenuesH :: IDType -> Maybe Int -> Maybe Int -> Maybe Token -> HandlerT IO [Venue]
getUsersIdVenuesH ui mLimit mOffset mToken = do
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
  where
    l = maybe 10 fromIntegral mLimit
    o = maybe 0 fromIntegral mOffset

getNewFbUser :: (MonadBaseControl IO m, MonadResource m) =>  Fb.UserAccessToken -> Fb.Credentials -> Manager -> m User
getNewFbUser userAT creds manager =  do
  fbUser <- Fb.runFacebookT creds manager $ Fb.getUser (accessTokenUserId userAT) [("fields", "id,name,email,first_name,last_name,picture.type(large){url}")] (Just userAT)
  return (fromFbUser fbUser)

getFbEvents :: (MonadBaseControl IO m, MonadResource m) =>  Fb.UserAccessToken -> Fb.Credentials -> Manager -> Int -> m [(Model.Event, Fb.RSVP)]
getFbEvents userAT creds manager limit = do
  let url = "/v2.8/" <> (Fb.idCode $ accessTokenUserId userAT) <> "/" <> "events"
  eventPager <- Fb.runFacebookT creds manager $ Fb.getObject url [("fields", "id,name,category,description,start_time,end_time,place,rsvp_status,owner,cover.fields(id,source),attending_count,maybe_count,declined_count")] (Just userAT)
  eventInvitedPager <- Fb.runFacebookT creds manager $ Fb.getObject url [("fields", "id,name,category,description,start_time,end_time,place,rsvp_status,owner,cover.fields(id,source),attending_count,maybe_count,declined_count"), ("type", "not_replied")] (Just userAT)
  es <- map (\e -> (fromFbEvent e, fromMaybe Fb.Unsure (traceShowId (Fb.eventRSVP e)))) <$> go eventPager []
  es' <- map (\e -> (fromFbEvent e, fromMaybe Fb.Unsure (traceShowId (Fb.eventRSVP e)))) <$> go eventInvitedPager []
  return (es ++ es')
  where go pager res =
          if P.length res < limit
          then do
            (Fb.runFacebookT creds manager $ Fb.fetchNextPage pager) >>= \case
              Just nextPager -> go nextPager
                (res ++ (Fb.pagerData pager))
              Nothing -> return res
          else return res

getFbVenue userAT creds manager venueId = do
  let url = "/v2.8/" <> venueId
  venue <- Fb.runFacebookT creds manager $ Fb.getObject url [("fields","id,name,about,emails,cover.fields(id,source),picture.type(normal),location,category")] (Just userAT)
  return (fromFbVenue venue)

getUsersIdSuggestedH :: IDType -> Maybe Double -> Maybe Double -> Maybe Double -> Maybe Int -> Maybe Int -> Maybe Token -> HandlerT IO [Api.Event]
getUsersIdSuggestedH ui (Just lat) (Just lon) distance' mLimit mOffset mToken = do
  $logInfo $ "Request suggested events of user with id: " <> (pack . show) ui <> " with lat, lon, dist: " <> (pack . show) [lat, lon, distance]
  creds' <- asks creds
  manager' <- asks manager
  token <- case mToken of
    Nothing -> throwError err402
    Just t -> return t
  es <- runQuery $ do
    checkExistsOrThrow ui
    us <- fromUser
    fs <- fromFriends
    ns <- fromNearby creds' manager' token
    return $ us ++ fs ++ ns
  runQuery $ mapM (intoApiEvent ui) es
  where
    fromUser = map entityVal <$> usersEvents ui
    fromFriends = map entityVal <$> friendsEvents ui
    fromNearby creds' manager' token = do
      now <- liftIO TI.getCurrentTime
      posixTime <- liftIO getPOSIXTime
      let userAT = Fb.UserAccessToken ui token now
      ves <- liftIO $ runResourceT $ Fb.runFacebookT creds' manager' (getVenuesAndEventsNearby userAT lat lon distance 1000 (Just (truncate posixTime)))
      mapM_ (\(v, _) -> insertBy v) ves
      mapM_ (\(_, es) -> mapM_ (insertBy . fromFbEvent) es) ves
      return $ P.concat $ map (\(_, es) -> map fromFbEvent es) ves

    distance = fromMaybe 1000 distance'
    l = maybe 10 fromIntegral mLimit
    o = maybe 0 fromIntegral mOffset
getUsersIdSuggestedH _ _ _ _ _ _ _ = throwError err400


getUsersIdDevicesH ui mToken = do
  $logInfo $ "Request get all user devices for: " <> (pack.show) ui
  ui' <- tokenUser mToken
  runQuery $ do
    checkExistsOrThrow ui
    checkExistsOrThrowError ui' err401
    when (ui' /= ui) (throwError err403)
    map entityVal <$> selectList [DeviceUserId ==. ui] []

putUsersIdDevicesIdH ui device@(Device t du di) mToken = do
  $logInfo $ "Request put user device id: " <> (pack.show) ui <> ", " <> (pack.show) di
  ui' <- tokenUser mToken
  runQuery $ do
    checkExistsOrThrow ui
    checkExistsOrThrowError ui' err401
    when (ui' /= ui) (throwError err403)
    when (ui' /= (deviceUserId device)) (throwError err403)
    insert_ device
    return device

deleteUsersIdDevicesIdH ui device@(Device t du di) mToken = do
  $logInfo $ "Request delete user device id: " <> (pack.show) ui <> ", " <> (pack.show) di
  ui' <- tokenUser mToken
  runQuery $ do
    checkExistsOrThrow ui
    checkExistsOrThrowError ui' err401
    when (ui' /= ui) (throwError err403)
    when (ui' /= du) (throwError err403)
    let uniqueDevice = (UniqueUserDevice t du di)
    getBy uniqueDevice >>= \case
      Just _ -> deleteBy uniqueDevice >> return device
      _ -> throwError err404

getUsersIdAgendaH :: IDType -> Maybe Int -> Maybe Int -> Maybe Token ->HandlerT IO [Api.Event]
getUsersIdAgendaH ui mLimit mOffset mToken = do
  $logInfo $ "Request agenda: " <> (pack.show) ui
  ui' <- tokenUser mToken
  runQuery $ do
    checkExistsOrThrow ui
    checkExistsOrThrowError ui' err401
    when (ui' /= ui) (throwError err403)
    eventEntities <- E.select $
      E.from
      $ \(event `E.InnerJoin` rsvp) -> do
      E.on (event^.EventFbID E.==. rsvp^.EventRSVPEventID
           E.&&. rsvp^.EventRSVPUserID E.==. (E.val ui)
           E.&&. (rsvp^.EventRSVPRsvp E.==. (E.val Fb.Attending)
                E.||. (rsvp^.EventRSVPRsvp E.==. (E.val Fb.Created))
                E.||. (rsvp^.EventRSVPRsvp E.==. (E.val Fb.Maybe))))
      return event
    let es = map entityVal eventEntities
    mapM (intoApiEvent ui) es
  where
    l = maybe 10 fromIntegral mLimit
    o = maybe 0 fromIntegral mOffset

getUsersIdPromotionsH :: IDType -> Maybe Token -> HandlerT IO [Promotion]
getUsersIdPromotionsH ui mToken = do
  $logDebug $ "Request promotions for: " <> (pack.show) ui
  ui' <- tokenUser mToken
  entities <- runQuery $ do
    checkExistsOrThrow ui
    checkExistsOrThrowError ui' err401
    when (ui' /= ui) (throwError err403)
    select . from $ pure
  return $ P.map entityVal entities
