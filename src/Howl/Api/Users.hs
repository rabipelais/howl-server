{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Howl.Api.Users where

import Control.Lens
import Data.Proxy
import Data.Text
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Swagger
import           Data.Typeable              (Typeable)
import GHC.Generics
import Data.Time

import Servant.Swagger
import qualified Howl.Facebook as FB
import Database.Persist

import Howl.Types
import Howl.Models

import Servant.API

instance ToHttpApiData FB.Id where
  toUrlPiece = FB.idCode

instance FromHttpApiData FB.Id where
  parseUrlPiece = Right . FB.Id

type UsersAPI = UsersPost :<|> UsersIdGet :<|> UsersIdPut

type UsersPost = "users" :> ReqBody '[JSON] FB.UserAccessToken
                         :> Post '[JSON] User

type UsersIdGet = "users" :> Capture "userID" IDType
                          :> ReqBody '[JSON] FB.UserAccessToken
                          :> Get '[JSON] User

type UsersIdPut = "users" :> Capture "userID" IDType
                          :> ReqBody '[JSON] (Authenticated User)
                          :> Put '[JSON] User


instance ToSchema User where
  declareNamedSchema proxy = do
    return $ NamedSchema (Just "User") $
      (sketchSchema
       (User (FB.Id "10155182179270463") "theCaptain" "Jean-Luc" (Just "Picard") (Just "make-it-so@yahoo.com")))
      & required .~ ["fbID", "username", "firstName"]

instance ToParamSchema IDType

instance (ToSchema a) => ToSchema (Authenticated a)

instance ToSchema (FB.UserAccessToken) where
  declareNamedSchema proxy = do
    return $ NamedSchema (Just "UserAccessToken") $
      (sketchSchema (FB.UserAccessToken "10155182179270463" "EAACEdEose0cBAIM1ZBWcOfQl3Gw03XZCY1yxzQZAZCA1HUuaqfaIUmhWRWfZCtDafrX0n6VaU8dGUggn7H0dpGe93eFUfVg5Ew4HxDdjb5jbNuFFuqcMbiKpMPdZAeoZATBVF1j8R5xTrWjiFnDJYLcjuhZCTccZCPqMIwUXZBm0lPNwZDZD" (UTCTime (fromGregorian 2015 12 31) 0)))
      & description ?~ "`id` is the FB app user ID, `token` the user access token, and `expires` is the token expiration date in `%FT%T%z` format"
