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


isUserTokenValid :: (MonadBaseControl IO m, MonadResource m) => Fb.Credentials -> Fb.UserAccessToken -> Fb.FacebookT Fb.Auth m Bool
isUserTokenValid c@(Fb.Credentials name ai secret) u@(Fb.UserAccessToken ui t _) = do
  at <- Fb.getAppAccessToken
  dt <- Fb.debugToken at t
  return $ all ($ dt) [ maybeTrue (\x -> ai == x) . Fb.dtAppId
                      , maybeTrue id . Fb.dtIsValid
                      , maybeTrue (\x -> ui == x) . Fb.dtUserId]
  where
    maybeTrue = maybe False

radians :: (Floating x) => x -> x
radians x =  (x/180*pi)

haversine :: (Double, Double) -> (Double, Double) -> Double
haversine (lat1, lon1) (lat2, lon2) = r * c
  where
    phi1 = radians lat1
    phi2 = radians lat2
    dlat = radians (lat2 - lat1)
    dlon = radians (lon2 - lon1)
    a = (sin(dlat/2))^2 + cos(phi1) * cos(phi2) * (sin(dlon/2))^2
    c = 2 * (atan2 (sqrt a) (sqrt (1-a)))
    r = 6371e3; -- earth radius in metres
