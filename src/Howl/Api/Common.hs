{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Howl.Api.Common where

import           Prelude as P
import           Control.Lens
import           Data.Swagger    hiding (Header, email)
import           Data.Time
import           Data.Text
import           Data.Aeson
import           GHC.Generics
import           Servant.Swagger hiding (Header)

import qualified Howl.Facebook   as FB
import           Howl.Models
import           Howl.Types

data ConnectCard =
  ConnectCard { userID :: IDType
              , eventID :: IDType
              , eventName :: Text
              , eventCoverURL :: Maybe FilePath
              , friends :: [User]}
  deriving (Show, Eq, Read, Generic)

deriving instance ToJSON ConnectCard
deriving instance FromJSON ConnectCard

data ApiUser =
  ApiUser {
    fbID           :: IDType
  , username       :: Text
  , firstName      :: Text
  , lastName       :: Maybe Text
  , email          :: Maybe Text
  , profilePicPath :: Maybe FilePath
  , private        :: Bool
  , following      :: Maybe FollowStatus }
  deriving (Eq, Read, Show, Generic)

deriving instance ToJSON ApiUser

deriving instance FromJSON ApiUser

toApiUser :: Maybe FollowStatus -> User -> ApiUser
toApiUser follow u = ApiUser (userFbID u) (userUsername u) (userFirstName u) (userLastName u) (userEmail u) (userProfilePicPath u) (userPrivate u) follow

fromApiUser :: ApiUser -> User
fromApiUser u = User (fbID u) (username u) (firstName u) (lastName u) (email u) (profilePicPath u) (private u)

instance ToSchema ConnectCard where
  declareNamedSchema proxy =
    return $ NamedSchema (Just "ConnectCard") $
      sketchSchema
       (ConnectCard (FB.Id "23412345123") (FB.Id "109890804920") "Swaggity Spot" (Just "www.coolpix.zh") [User (FB.Id "10155182179270463") "theCaptain" "Jean-Luc" (Just "Picard") (Just "make-it-so@yahoo.com") Nothing True])
      & required .~ ["userID", "eventID", "eventName", "friends"]

instance ToSchema Venue where
  declareNamedSchema proxy =
    return $ NamedSchema (Just "Venue") $
      sketchSchema
       (Venue (FB.Id "10155182179270463") Nothing Nothing (Just "Nightlife") "Short summary" "The sweetest place in town" "Swaggy House"(Just "Karlsruhe") (Just "Germany") (Just "Karlstraße") (Just "76131") (Just 40.08031) (Just (-70.87123)) (Just 3.3))
      & required .~ ["fbID", "description", "name"]

deriving instance ToSchema FollowStatus

instance ToSchema Device where
  declareNamedSchema proxy =
    return $ NamedSchema (Just "Device") $
      sketchSchema
      (Device Android (FB.Id "10155182179270463") "ARITN01909830190-iniasortn#$#@inrtsinars")
      & required .~ ["type", "userId", "deviceId"]

instance ToParamSchema DeviceType

instance ToSchema Invite where
  declareNamedSchema proxy =
    return $ NamedSchema (Just "Invite") $
      sketchSchema
       (Invite (FB.Id "10155182179270463") (FB.Id "81731981379127") (FB.Id "81787816817678"))
      & required .~ ["sourceID", "targetID", "eventID"]

instance ToSchema EventRSVP where
  declareNamedSchema proxy =
    return $ NamedSchema (Just "EventRSVP") $
      sketchSchema
       (EventRSVP (FB.Id "10155182179270463") (FB.Id "81731981379127") FB.Attending)
      & required .~ ["userID", "eventID", "rsvp"]

deriving instance ToSchema FB.RSVP

instance ToSchema User where
  declareNamedSchema proxy =
    return $ NamedSchema (Just "User") $
      sketchSchema
       (User (FB.Id "10155182179270463") "theCaptain" "Jean-Luc" (Just "Picard") (Just "make-it-so@yahoo.com") Nothing True)
      & required .~ ["fbID", "username", "firstName"]

instance ToSchema ApiUser where
  declareNamedSchema proxy =
    return $ NamedSchema (Just "ApiUser") $
      sketchSchema
       (ApiUser (FB.Id "10155182179270463") "theCaptain" "Jean-Luc" (Just "Picard") (Just "make-it-so@yahoo.com") Nothing True (Just Blocked))
      & required .~ ["fbID", "username", "firstName"]

instance ToSchema VenueFollower where
  declareNamedSchema proxy =
    return $ NamedSchema (Just "VenueFollower") $
      sketchSchema
       (VenueFollower (FB.Id "10155182179270463") (FB.Id "097955182179125125"))
      & required .~ ["venueID", "userID"]

instance ToSchema Event where
  declareNamedSchema proxy =
    return $ NamedSchema (Just "Event") $
      sketchSchema
       (Event (FB.Id "10155182179270463") "Fun swaggy loooooong party." "All You Can Swag" 0 0 0 (UTCTime (fromGregorian 2015 12 31) 0) (UTCTime (fromGregorian 2515 12 31) 0) (FB.Id "901579654279270463") (Just "www.coolpic.gov"))
      & required .~ ["fbID", "description", "name", "startTime", "endTime", "venueId"]

instance ToSchema IDType where
  declareNamedSchema proxy =
    return $ NamedSchema Nothing $ mempty
      & type_ .~ SwaggerString

instance ToParamSchema IDType

instance ToSchema (FB.UserAccessToken) where
  declareNamedSchema proxy =
    return $ NamedSchema (Just "UserAccessToken") $
      sketchSchema (FB.UserAccessToken "10155182179270463" "EAACEdEose0cBAIM1ZBWcOfQl3Gw03XZCY1yxzQZAZCA1HUuaqfaIUmhWRWfZCtDafrX0n6VaU8dGUggn7H0dpGe93eFUfVg5Ew4HxDdjb5jbNuFFuqcMbiKpMPdZAeoZATBVF1j8R5xTrWjiFnDJYLcjuhZCTccZCPqMIwUXZBm0lPNwZDZD" (UTCTime (fromGregorian 2015 12 31) 0))
      & description ?~ "`id` is the FB app user ID, `token` the user access token, and `expires` is the token expiration date in `%FT%T%z` format"
