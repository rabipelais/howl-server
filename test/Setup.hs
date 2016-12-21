module Setup where

import Howl
import qualified Facebook as Fb

getCreds token = do
  expires <- addUTCTime 400000000 <$> getCurrentTime
  manager <- newManager tlsManagerSettings
  let userAT = Fb.UserAccessToken "10155182179270463" token expires
  (Just creds) <- getCredentials "../app.cfg"
  return (manager, userAT, creds)
