{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Howl.Api.Common where

import           Prelude as P
import           Control.Lens
import           Data.Swagger    hiding (Header, email, name)
import           Data.Time
import           Data.Text
import           Data.Aeson
import           GHC.Generics
import           Servant.Swagger hiding (Header)

import qualified Howl.Facebook   as FB
import           Howl.Models hiding (Event, ConnectItem)
import qualified Howl.Models     as M
import           Howl.Types

data ApiUser =
  ApiUser {
    fbID           :: IDType
  , name           :: Text
  , username       :: Text
  , firstName      :: Maybe Text
  , lastName       :: Maybe Text
  , email          :: Maybe Text
  , profilePicPath :: Maybe FilePath
  , private        :: Bool
  , following      :: Maybe FollowStatus }
  deriving (Eq, Read, Show, Generic)

deriving instance ToJSON ApiUser

deriving instance FromJSON ApiUser

toApiUser :: Maybe FollowStatus -> User -> ApiUser
toApiUser follow u = ApiUser (userFbID u) (userName u) (userUsername u) (userFirstName u) (userLastName u) (userEmail u) (userProfilePicPath u) (userPrivate u) follow

fromApiUser :: ApiUser -> User
fromApiUser u = User (fbID u) (name u) (username u) (firstName u) (lastName u) (email u) (profilePicPath u) (private u)

data Event =
  Event { event :: M.Event
        , tag :: Text
        , friendsGoing :: Int
        , namesGoing :: [Text]
        , picsGoing :: [FilePath]
        , friendsInterested :: Int
        , namesInterested :: [Text]
        , picsInterested :: [FilePath] }
  deriving (Eq, Read, Show, Generic)

deriving instance ToJSON Event

deriving instance FromJSON Event

data ConnectItem =
  ConnectItem { connectId :: IDType
              , eventId :: IDType
              , event :: Event
              , friends :: [ApiUser]}
  deriving (Eq, Read, Show, Generic)

deriving instance ToJSON ConnectItem
deriving instance FromJSON ConnectItem

instance ToSchema ConnectItem where
  declareNamedSchema proxy =
    return $ NamedSchema (Just "ConnectItem") $
    sketchSchema
    (ConnectItem (FB.Id "23412345123") (FB.Id "109890804920") (Event mockEvent "Suggested" 1 ["Juan"] ["coolurl.com"] 2 ["Carmen", "Bob"] ["not-cool-url.gov", "picpath"]) [])
    & required .~ ["connectId", "eventId", "event", "friends"]

instance ToSchema M.Notification where
  declareNamedSchema proxy =
    return $ NamedSchema (Just "Notification") $
    sketchSchema
    (Notification (FB.Id "23412345123") (FB.Id "109890804920") (FB.Id "234123455432") (UTCTime (fromGregorian 2015 12 31) 0) Invitation)
    & required .~ ["userID", "source", "target", "createdAt", "type"]

instance ToSchema Promotion where
  declareNamedSchema proxy =
    return $ NamedSchema (Just "Promotion") $
      sketchSchema
       (Promotion (Just "fancyurl.web") "Dope Promotion" (Just "Awesome description") (Just "Kool Place") (Just "26357137137813541687"))
      & required .~ ["name"]

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
       (User (FB.Id "10155182179270463") "Jean-Luc Picard" "theCaptain" (Just "Jean-Luc") (Just "Picard") (Just "make-it-so@yahoo.com") Nothing True)
      & required .~ ["fbID", "username", "firstName"]

instance ToSchema ApiUser where
  declareNamedSchema proxy =
    return $ NamedSchema (Just "ApiUser") $
      sketchSchema
       (ApiUser (FB.Id "10155182179270463") "Jean-Luc Picard" "theCaptain" (Just "Jean-Luc") (Just "Picard") (Just "make-it-so@yahoo.com") Nothing True (Just Blocked))
      & required .~ ["fbID", "username", "firstName"]

instance ToSchema VenueFollower where
  declareNamedSchema proxy =
    return $ NamedSchema (Just "VenueFollower") $
      sketchSchema
       (VenueFollower (FB.Id "10155182179270463") (FB.Id "097955182179125125"))
      & required .~ ["venueID", "userID"]

mockEvent = (M.Event (FB.Id "10155182179270463") "Fun swaggy loooooong party." "All You Can Swag" 0 0 0 (UTCTime (fromGregorian 2015 12 31) 0) (UTCTime (fromGregorian 2515 12 31) 0) (FB.Id "901579654279270463") (Just "www.coolpic.gov"))

instance ToSchema M.Event where
  declareNamedSchema proxy =
    return $ NamedSchema (Just "Event") $
      sketchSchema
      mockEvent
      & required .~ ["fbID", "description", "name", "startTime", "endTime", "venueId"]


instance ToSchema Event where
  declareNamedSchema proxy =
    return $ NamedSchema (Just "Api.Event") $
      sketchSchema
       (Event mockEvent "Suggested" 1 ["Juan"] ["coolurl.com"] 2 ["Carmen", "Bob"] ["not-cool-url.gov", "picpath"])
      & required .~ [ "event", "tag"
                    , "friendsGoing", "namesGoing", "picsGoing"
                    , "friendsInterested", "namesInterested"
                    , "picsInterested"]

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
