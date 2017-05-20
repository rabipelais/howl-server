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

import qualified Data.Time.Clock            as T
import           Database.Persist
import qualified Howl.Facebook              as FB

import           Howl.Api.Common            as Api
import           Howl.Models                as Model
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
  :<|> UsersIdFollowersGet
  :<|> UsersIdFollowsPost
  :<|> UsersIdFollowsIdGet
  :<|> UsersIdFollowsIdDelete
  :<|> UsersIdFollowingCountGet
  :<|> UsersIdFollowersCountGet
  :<|> UsersIdBlockedGet
  :<|> UsersIdBlockedPost
  :<|> UsersIdBlockedFollowsIdDelete
  :<|> UsersIdEventsGet
  :<|> UsersIdEventsFollowsGet
  :<|> UsersIdVenuesGet
  :<|> UsersIdSuggestedGet
  :<|> UsersIdHappeningGet
  :<|> UsersIdDevicesGet
  :<|> UsersIdDevicesIdPut
  :<|> UsersIdDevicesIdDelete
  :<|> UsersIdAgendaGet
  :<|> UsersIdPromotionsGet
  :<|> UsersIdNotificationsGet


type UsersGet = "users" :> Header "token" Token
                        :> Get '[JSON] [User]

type UsersPost = "users" :> ReqBody '[JSON] FB.UserAccessToken
                         :> PostCreated '[JSON] User

type UsersPut = "users" :> ReqBody '[JSON] User
                        :> Header "token" Token
                        :> Put '[JSON] User

type UsersIdGet = "users" :> Capture "userID" IDType
                          :> Header "token" Token
                          :> Get '[JSON] ApiUser

type UsersIdPut = "users" :> Capture "userID" IDType
                          :> ReqBody '[JSON] User
                          :> Header "token" Token
                          :> Put '[JSON] User

type UsersIdConnectGet = "users" :> Capture "userID" IDType
                       :> "connect"
                       :> Header "token" Token
                       :> Get '[JSON] [ConnectCard]

type UsersIdDelete = "users" :> Capture "userID" IDType
                          :> Header "token" Token
                          :> Delete '[JSON] IDType

type UsersIdFollowsGet = "users" :> Capture "userID" IDType
                          :> "follows"
                          :> QueryParam "limit" Int
                          :> QueryParam "offset" Int
                          :> Header "token" Token
                          :> Get '[JSON] [ApiUser]

type UsersIdFollowersGet = "users" :> Capture "userID" IDType
                          :> "followers"
                          :> QueryParam "limit" Int
                          :> QueryParam "offset" Int
                          :> Header "token" Token
                          :> Get '[JSON] [ApiUser]

type UsersIdFollowsPost = "users" :> Capture "userID" IDType
                          :> "follows"
                          :> Capture "friendID" IDType
                          :> Header "token" Token
                          :> PostAccepted '[JSON] IDType

type UsersIdEventsFollowsGet = "users" :> Capture "userID" IDType
                          :> "events" :> "follows"
                          :> QueryParam "limit" Int
                          :> QueryParam "offset" Int
                          :> Header "token" Token
                          :> Get '[JSON] [Api.Event]

type UsersIdFollowsIdGet = "users" :> Capture "userID" IDType
                           :> "follows"
                           :> Capture "friendID" IDType
                           :> Header "token" Token
                           :> Get '[JSON] FollowStatus

type UsersIdFollowingCountGet = "users" :> Capture "userID" IDType
                           :> "following"
                           :> "count"
                           :> Header "token" Token
                           :> Get '[JSON] Int

type UsersIdFollowersCountGet = "users" :> Capture "userID" IDType
                           :> "followers"
                           :> "count"
                           :> Header "token" Token
                           :> Get '[JSON] Int

type UsersIdFollowsIdDelete = "users" :> Capture "userID" IDType
                              :> "follows"
                              :> Capture "friendID" IDType
                              :> Header "token" Token
                              :> Delete '[JSON] IDType

type UsersIdBlockedGet = "users" :> Capture "userID" IDType
                         :> "blocked"
                         :> QueryParam "limit" Int
                         :> QueryParam "offset" Int
                         :> Header "token" Token
                         :> Get '[JSON] [ApiUser]

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
                        :> QueryParam "limit" Int
                        :> QueryParam "offset" Int
                        :> Header "token" Token
                        :> Get '[JSON] [Api.Event]

type UsersIdVenuesGet = "users" :> Capture "userID" IDType
                        :> "venues"
                        :> QueryParam "limit" Int
                        :> QueryParam "offset" Int
                        :> Header "token" Token
                        :> Get '[JSON] [Venue]

type UsersIdSuggestedGet = "users" :> Capture "userID" IDType
                           :> "suggested"
                           :> QueryParam "lat" Double
                           :> QueryParam "lon" Double
                           :> QueryParam "distance" Double
                           :> QueryParam "limit" Int
                           :> QueryParam "offset" Int
                           :> QueryParam "startDay" T.UTCTime
                           :> QueryParam "limitDays" Int
                           :> QueryParam "limitPerDay" Int
                           :> Header "token" Token
                           :> Get '[JSON] [Api.Event]

type UsersIdHappeningGet = "users" :> Capture "userID" IDType
                           :> "happening"
                           :> QueryParam "lat" Double
                           :> QueryParam "lon" Double
                           :> QueryParam "distance" Double
                           :> QueryParam "limit" Int
                           :> QueryParam "offset" Int
                           :> Header "token" Token
                           :> Get '[JSON] [Api.Event]

type UsersIdDevicesGet = "users" :> Capture "userID" IDType
                        :> "devices"
                        :> Header "token" Token
                        :> Get '[JSON] [Device]

type UsersIdDevicesIdPut = "users" :> Capture "userID" IDType
                        :> "devices"
                        :> ReqBody '[JSON] Device
                        :> Header "token" Token
                        :> Put '[JSON] Device

type UsersIdDevicesIdDelete = "users" :> Capture "userID" IDType
                        :> "devices"
                        :> ReqBody '[JSON] Device
                        :> Header "token" Token
                        :> Delete '[JSON] Device

type UsersIdAgendaGet = "users" :> Capture "userID" IDType
                        :> "agenda"
                        :> QueryParam "limit" Int
                        :> QueryParam "offset" Int
                        :> Header "token" Token
                        :> Get '[JSON] [Api.Event]

type UsersIdPromotionsGet = "users" :> Capture "userID" IDType
                        :> "promotions"
                        :> Header "token" Token
                        :> Get '[JSON] [Promotion]

type UsersIdNotificationsGet = "users" :> Capture "userID" IDType
                        :> "notifications"
                        :> QueryParam "limit" Int
                        :> QueryParam "offset" Int
                        :> Header "token" Token
                        :> Get '[JSON] [Notification]
