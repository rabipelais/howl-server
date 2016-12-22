{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Howl.Utils where

import qualified Howl.Facebook as Fb
import Network.HTTP.Conduit (Manager)
import Data.Maybe
import Control.Monad.Trans.Resource
import Data.Monoid ((<>))
import Data.Serialize

import Howl.Models


-- | Get the user ID of an user access token.
accessTokenUserId :: Fb.UserAccessToken -> Fb.UserId
accessTokenUserId (Fb.UserAccessToken uid _ _) = uid

getNewUser :: (MonadBaseControl IO m, MonadResource m) =>  Fb.UserAccessToken -> Fb.Credentials -> Manager -> m User
getNewUser userAT creds manager =  do
  fbUser <- Fb.runFacebookT creds manager $ Fb.getUser (accessTokenUserId userAT) [("fields", "id,name,email,first_name,last_name")] (Just userAT)
  let
    fbID = Fb.userId fbUser
    username = fromMaybe (Fb.idCode fbID) (Fb.userUsername fbUser)
    profilePicPath = Nothing
    firstName = fromMaybe username (Fb.userFirstName fbUser)
    lastName = Fb.userLastName fbUser
    email = Fb.userEmail fbUser
    user = User fbID username profilePicPath firstName lastName email
  return user
