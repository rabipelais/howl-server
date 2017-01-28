{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Howl.Api.Users where

import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Proxy
import           Data.Text
import           Data.Typeable              (Typeable)
import           GHC.Generics

import           Database.Persist
import qualified Howl.Facebook              as FB

import           Howl.Api.Common
import           Howl.Models
import           Howl.Types

import           Servant.API

type UsersAPI =
  UsersGet
  :<|> UsersPost
  :<|> UsersPut
  :<|> UsersIdGet
  :<|> UsersIdPut
  :<|> UsersIdDelete
  :<|> UsersIdConnectGet
  :<|> UsersIdFollowsGet
  :<|> UsersIdFollowsPost
  :<|> UsersIdFollowsIdGet
  :<|> UsersIdFollowsIdDelete
  :<|> UsersIdBlockedGet
  :<|> UsersIdBlockedPost
  :<|> UsersIdBlockedFollowsIdDelete
  :<|> UsersIdEventsGet
  :<|> UsersIdEventsFollowsGet
  :<|> UsersIdVenuesGet
  :<|> UsersIdSuggestedGet


type UsersGet = "users" :> Header "token" Token
                        :> Get '[JSON] [User]

type UsersPost = "users" :> ReqBody '[JSON] FB.UserAccessToken
                         :> PostCreated '[JSON] User

type UsersPut = "users" :> ReqBody '[JSON] User
                        :> Header "token" Token
                        :> Put '[JSON] User

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

type UsersIdFollowsGet = "users" :> Capture "userID" IDType
                          :> "follows"
                          :> Header "token" Token
                          :> Get '[JSON] [User]

type UsersIdFollowsPost = "users" :> Capture "userID" IDType
                          :> "follows"
                          :> ReqBody '[JSON] IDType
                          :> Header "token" Token
                          :> PostAccepted '[JSON] IDType

type UsersIdEventsFollowsGet = "users" :> Capture "userID" IDType
                          :> "events" :> "follows"
                          :> Header "token" Token
                          :> Get '[JSON] [Event]

type UsersIdFollowsIdGet = "users" :> Capture "userID" IDType
                           :> "follows"
                           :> Capture "friendID" IDType
                           :> Header "token" Token
                           :> Get '[JSON] FollowStatus

type UsersIdFollowsIdDelete = "users" :> Capture "userID" IDType
                              :> "follows"
                              :> Capture "friendID" IDType
                              :> Header "token" Token
                              :> Delete '[JSON] IDType

type UsersIdBlockedGet = "users" :> Capture "userID" IDType
                         :> "blocked"
                         :> Header "token" Token
                         :> Get '[JSON] [User]

type UsersIdBlockedPost = "users" :> Capture "userID" IDType
                          :> "blocked"
                          :> ReqBody '[JSON] IDType
                          :> Header "token" Token
                          :> PostCreated '[JSON] IDType

type UsersIdBlockedFollowsIdDelete = "users" :> Capture "userID" IDType
                                     :> "blocked"
                                     :> Capture  "friendID" IDType
                                     :> Header "token" Token
                                     :> Delete '[JSON] IDType

type UsersIdEventsGet = "users" :> Capture "userID" IDType
                        :> "events"
                        :> Header "token" Token
                        :> Get '[JSON] [Event]

type UsersIdVenuesGet = "users" :> Capture "userID" IDType
                        :> "venues"
                        :> Header "token" Token
                        :> Get '[JSON] [Venue]

type UsersIdSuggestedGet = "users" :> Capture "userID" IDType
                           :> "suggested"
                           :> QueryParam "lat" Double
                           :> QueryParam "lon" Double
                           :> QueryParam "distance" Double
                           :> Header "token" Token
                           :> Get '[JSON] [Event]
