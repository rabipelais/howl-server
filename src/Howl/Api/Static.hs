{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Howl.Api.Static where

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
import           Servant.RawM               (RawM)

type StaticAPI = "static" :> RawM
