{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Howl.Api where

import           Data.Proxy
import           Data.Text

import           Database.Persist
import qualified Howl.Facebook    as FB

import           Howl.Api.Events
import           Howl.Api.Search
import           Howl.Api.Static
import           Howl.Api.Users
import           Howl.Api.Venues
import           Howl.Models

import           Servant.API

type ApiV1 = "v1" :> (UsersAPI :<|> EventsAPI :<|> VenuesAPI :<|> SearchAPI)
type Api = ApiV1
type ApiRaw = Api :<|> StaticAPI

api :: Proxy ApiRaw
api = Proxy
