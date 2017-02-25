{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeOperators    #-}

module Howl.App where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Network.HTTP.Conduit       (Manager, newManager,
                                             tlsManagerSettings)

import qualified Howl.Facebook              as Fb

import           Servant

import           Data.Text

import           Howl.Api
import           Howl.App.Events
import           Howl.App.Search
import           Howl.App.Static
import           Howl.App.Users
import           Howl.App.Venues
import           Howl.Models
import           Howl.Monad
import           Howl.Types
import           Howl.Utils

handlers :: ServerT Api (HandlerT IO)
handlers = usersHandlers :<|> eventsHandlers :<|> venuesHandlers :<|> searchHandlers

server :: LogEnv HandlerEnv -> Server ApiRaw
server env = enter dienerToEither handlers :<|> staticHandlers
  where
    dienerToEither :: HandlerT IO :~> ExceptT ServantErr IO
    dienerToEither = Nat $ \ar ->
      liftIO (runDienerT env ar) >>= \case
        Left err -> throwError err
        Right a -> return a

app :: LogEnv HandlerEnv -> Application
app = serve api . server
