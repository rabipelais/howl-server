{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Howl.Api where

import Data.Proxy
import Data.Text

import qualified Facebook as FB
import Database.Persist

import Howl.Models

import Servant.API

instance ToHttpApiData FB.Id where
  toUrlPiece = FB.idCode

instance FromHttpApiData FB.Id where
  parseUrlPiece = Right . FB.Id

type Api =
       "user" :> ReqBody '[JSON] FB.UserAccessToken :> Post '[JSON] (Maybe (Key User))
  :<|> "user" :> Capture "userID" IDType  :> Get  '[JSON] (Maybe User)

api :: Proxy Api
api = Proxy
