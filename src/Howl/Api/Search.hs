{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Howl.Api.Search where

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

type SearchAPI =
  SearchUsersGet
  :<|> SearchEventsGet
  :<|> SearchVenuesGet

type SearchUsersGet = "search" :> "users"
                    :> QueryParam "q" Text
                    :> Header "token" Token
                    :> Get '[JSON] [User]

type SearchEventsGet = "search" :> "events"
                    :> QueryParam "q" Text
                    :> Header "token" Token
                    :> Get '[JSON] [Event]

type SearchVenuesGet = "search" :> "venues"
                    :> QueryParam "q" Text
                    :> Header "token" Token
                    :> Get '[JSON] [Venue]
