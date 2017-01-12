{-# LANGUAGE FlexibleContexts #-}

module Howl.App where

import           Control.Monad.IO.Class
import           Control.Monad.Logger     (runStderrLoggingT)

import           Data.String.Conversions

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Network.HTTP.Conduit     (Manager, newManager,
                                           tlsManagerSettings)
import           Network.Wai
import           Network.Wai.Handler.Warp as Warp

import qualified Howl.Facebook            as Fb

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

sizeOfSqlitePool = 10

mkApp :: FilePath -> Fb.Credentials -> IO Application
mkApp sqliteFile fbCredentials = do
  pool <- runStderrLoggingT $ createSqlitePool (cs sqliteFile) sizeOfSqlitePool
  runSqlPool (runMigration migrateAll) pool
  manager <- newManager tlsManagerSettings
  return $ app pool manager fbCredentials

run sqlFile fbCredentials = Warp.run 3000 =<< mkApp sqlFile fbCredentials
