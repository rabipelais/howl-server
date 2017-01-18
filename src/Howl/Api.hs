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
import           Howl.Api.Users
import           Howl.Models

import           Servant.API

type ApiV1 = "v1" :> (UsersAPI :<|> EventsAPI)
type Api = ApiV1

api :: Proxy Api
api = Proxy
