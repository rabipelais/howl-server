{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Howl.Utils where

import           Control.Monad.Trans.Resource
import           Data.Maybe
import           Data.Monoid                  ((<>))
import           Data.Serialize
import           Data.Time
import qualified Howl.Facebook                as Fb
import           Network.HTTP.Conduit         (Manager)

import           Howl.Models

-- | Get the user ID of an user access token.
accessTokenUserId :: Fb.UserAccessToken -> Fb.UserId
accessTokenUserId (Fb.UserAccessToken uid _ _) = uid

laterToken i t = do
  now <- getCurrentTime
  let later = addUTCTime 3600 now
  return $ Fb.UserAccessToken i t later

getNewUser :: (MonadBaseControl IO m, MonadResource m) =>  Fb.UserAccessToken -> Fb.Credentials -> Manager -> m User
getNewUser userAT creds manager =  do
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


isUserTokenValid :: (MonadBaseControl IO m, MonadResource m) => Fb.Credentials -> Fb.UserAccessToken -> Fb.FacebookT Fb.Auth m Bool
isUserTokenValid c@(Fb.Credentials name ai secret) u@(Fb.UserAccessToken ui t _) = do
  at <- Fb.getAppAccessToken
  dt <- Fb.debugToken at t
  return $ and $ map ($ dt) [ maybeTrue (\x -> ai == x) . Fb.dtAppId
                            , maybeTrue id . Fb.dtIsValid
                            , maybeTrue (\x -> ui == x) . Fb.dtUserId]
  where
    maybeTrue = maybe False
