{-# LANGUAGE FlexibleContexts #-}

module Howl.App where

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Network.HTTP.Conduit    (Manager, newManager,
                                          tlsManagerSettings)

import qualified Howl.Facebook           as Fb

import           Servant

import           Data.Text

import           Howl.Api
import           Howl.Models
import           Howl.Utils

import           Howl.App.Users

server :: (ConnectionPool, Manager, Fb.Credentials) -> Server Api
server (p, m, c) = usersHandlers (p, m, c)

app :: ConnectionPool -> Manager -> Fb.Credentials -> Application
app pool manager fbCredentials = serve api $ server (pool, manager, fbCredentials)
