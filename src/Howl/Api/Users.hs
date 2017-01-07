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
import           Data.Swagger hiding (Header)
import           Data.Typeable              (Typeable)
import GHC.Generics
import Data.Time

import Servant.Swagger hiding (Header)
import qualified Howl.Facebook as FB
import Database.Persist

import Howl.Types
import Howl.Models

import Servant.API

type UsersAPI =
  UsersGet
  :<|> UsersPost
  :<|> UsersIdGet
  :<|> UsersIdPut
  :<|> UsersIdDelete
                -- :<|> UsersIdConnectGet
                -- :<|> UsersIdFriendsGet
                -- :<|> UsersIdFriendsPost
                -- :<|> UsersIdFriendsEventsGet
                -- :<|> UsersIdFriendsIdDelete
                -- :<|> UsersIdEventsGet


type UsersGet = "users" :> Header "token" Token
                        :> Get '[JSON] [User]

type UsersPost = "users" :> ReqBody '[JSON] FB.UserAccessToken
                         :> PostCreated '[JSON] User

type UsersIdGet = "users" :> Capture "userID" IDType
                          :> Header "token" Token
                          :> Get '[JSON] User

type UsersIdPut = "users" :> Capture "userID" IDType
                          :> ReqBody '[JSON] User
                          :> Header "token" Token
                          :> Put '[JSON] User

type UsersIdConnectGet = "users" :> Capture "userID" IDType
                       :> "connect"
                       :> Header "token" Token
                       :> Get '[JSON] [User]

type UsersIdDelete = "users" :> Capture "userID" IDType
                          :> Header "token" Token
                          :> Delete '[JSON] IDType

type UsersIdFriendsGet = "users" :> Capture "userID" IDType
                          :> "friends"
                          :> Header "token" Text
                          :> Get '[JSON] [User]

type UsersIdFriendsPost = "users" :> Capture "userID" IDType
                          :> "friends"
                          :> ReqBody '[JSON] IDType
                          :> Header "token" Text
                          :> PostAccepted '[JSON] IDType

type UsersIdFriendsEventsGet = "users" :> Capture "userID" IDType
                          :> "friends" :> "events"
                          :> Header "token" Text
                          :> Get '[JSON] [Event]

type UsersIdFriendsIdDelete = "users" :> Capture "userID" IDType
                              :> "friends"
                              :> Capture "friendID" IDType
                              :> Header "token" Text
                              :> Delete '[JSON] IDType

type UsersIdEventsGet = "users" :> Capture "userID" IDType
                        :> "events"
                        :> Header "token" Text
                        :> Get '[JSON] [Event]


instance ToSchema User where
  declareNamedSchema proxy = do
    return $ NamedSchema (Just "User") $
      (sketchSchema
       (User (FB.Id "10155182179270463") "theCaptain" "Jean-Luc" (Just "Picard") (Just "make-it-so@yahoo.com") Nothing))
      & required .~ ["fbID", "username", "firstName"]

instance ToSchema Event where
  declareNamedSchema proxy = do
    return $ NamedSchema (Just "Event") $
      (sketchSchema
       (Event (FB.Id "10155182179270463") "Fun swaggy party." "All You Can Swag" (UTCTime (fromGregorian 2015 12 31) 0) (UTCTime (fromGregorian 2515 12 31) 0) (FB.Id "901579654279270463")))
      & required .~ ["fbID", "description", "name", "startTime", "endTime", "venueId"]

instance ToSchema IDType where
  declareNamedSchema proxy = do
    return $ NamedSchema Nothing $ mempty
      & type_ .~ SwaggerString

instance ToParamSchema IDType

instance (ToSchema a) => ToSchema (Authenticated a)

instance ToSchema (FB.UserAccessToken) where
  declareNamedSchema proxy = do
    return $ NamedSchema (Just "UserAccessToken") $
      (sketchSchema (FB.UserAccessToken "10155182179270463" "EAACEdEose0cBAIM1ZBWcOfQl3Gw03XZCY1yxzQZAZCA1HUuaqfaIUmhWRWfZCtDafrX0n6VaU8dGUggn7H0dpGe93eFUfVg5Ew4HxDdjb5jbNuFFuqcMbiKpMPdZAeoZATBVF1j8R5xTrWjiFnDJYLcjuhZCTccZCPqMIwUXZBm0lPNwZDZD" (UTCTime (fromGregorian 2015 12 31) 0)))
      & description ?~ "`id` is the FB app user ID, `token` the user access token, and `expires` is the token expiration date in `%FT%T%z` format"
