{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Howl.Api.Users where

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

import Howl.Models

import Servant.API

instance ToHttpApiData FB.Id where
  toUrlPiece = FB.idCode

instance FromHttpApiData FB.Id where
  parseUrlPiece = Right . FB.Id

type UsersAPI = UsersPost :<|> UsersIdGet

type UsersPost = "users" :> ReqBody '[JSON] FB.UserAccessToken :> Post '[JSON] (Maybe User)

type UsersIdGet = "users" :> Capture "userID" IDType  :> Get  '[JSON] (Maybe User)


instance ToSchema User

instance ToSchema IDType
instance ToParamSchema IDType

instance ToSchema (FB.AccessToken FB.UserKind)
instance Generic (FB.AccessToken FB.UserKind) where
  type Rep (FB.AccessToken FB.UserKind) =
    D1 ('MetaData "AccessToken" "Users" "howl-backend" 'False)
    (C1 ('MetaCons "UserAccessToken" 'PrefixI 'False)
                    (S1 ('MetaSel 'Nothing
                         'NoSourceUnpackedness
                         'NoSourceStrictness
                         'DecidedLazy) (Rec0 FB.UserId) :*:
                     (S1 ('MetaSel 'Nothing
                         'NoSourceUnpackedness
                         'NoSourceStrictness
                         'DecidedLazy) (Rec0 FB.AccessTokenData)) :*:
                      (S1 ('MetaSel 'Nothing
                         'NoSourceUnpackedness
                         'NoSourceStrictness
                         'DecidedLazy) (Rec0 UTCTime))))
  from (FB.UserAccessToken ui atd t) = (M1 (M1 (M1 (K1 ui) :*: M1 (K1 atd) :*: M1 (K1 t))))
  to (M1 (M1 (M1 (K1 ui) :*: M1 (K1 atd) :*: M1 (K1 t)))) = FB.UserAccessToken ui atd t
