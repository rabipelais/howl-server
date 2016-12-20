{-# LANGUAGE OverloadedStrings #-}
module Main where

import Howl
import qualified Facebook as Fb
import Data.Configurator as C

--------------------------------------------------

getCredentials :: FilePath -> IO (Maybe Fb.Credentials)
getCredentials filePath = do
  config <- load [Required filePath]
  appName <- C.lookup config "credentials.appName"
  appId <- C.lookup config "credentials.appId"
  appSecret <- C.lookup config "credentials.appSecret"
  return $ Fb.Credentials <$> appName <*> appId <*> appSecret

main :: IO ()
main = do
  mCreds <- getCredentials "app.cfg"
  print "Starting server."
  case mCreds of
    Nothing -> print "ERROR reading config file"
    Just creds -> run "sqlitetest.db" creds
