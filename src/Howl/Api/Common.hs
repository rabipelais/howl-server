{-# LANGUAGE DeriveAnyClass #-}
module Howl.Api.Common where

import           Control.Lens
import           Data.Swagger    hiding (Header)
import           Data.Time
import           Servant.Swagger hiding (Header)

import qualified Howl.Facebook   as FB
import           Howl.Models
import           Howl.Types

instance ToSchema Venue where
  declareNamedSchema proxy =
    return $ NamedSchema (Just "Venue") $
      sketchSchema
       (Venue (FB.Id "10155182179270463") Nothing "Short summary" "The sweetest place in town" "Swaggy House"(Just "Karlsruhe") (Just "Germany") (Just "Karlstraße") (Just "76131") (Just 40.08031) (Just (-70.87123)) (Just 3.3))
      & required .~ ["fbID", "description", "name"]

deriving instance ToSchema FollowStatus

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
       (User (FB.Id "10155182179270463") "theCaptain" "Jean-Luc" (Just "Picard") (Just "make-it-so@yahoo.com") Nothing)
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
       (Event (FB.Id "10155182179270463") "Fun swaggy loooooong party." "All You Can Swag" (UTCTime (fromGregorian 2015 12 31) 0) (UTCTime (fromGregorian 2515 12 31) 0) (FB.Id "901579654279270463") (Just "www.coolpic.gov"))
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
