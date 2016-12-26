{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Howl.Api where

import Data.Proxy
import Data.Text

import qualified Howl.Facebook as FB
import Database.Persist

import Howl.Models
import Howl.Api.Users

import Servant.API

type ApiV1 = "v1" :> UsersAPI
type Api = ApiV1

api :: Proxy Api
api = Proxy
