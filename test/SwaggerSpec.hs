{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SwaggerSpec where

import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Swagger
import           Servant.Swagger
import           Servant.Swagger.Test
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances  ()

import           Howl
import qualified Howl.Facebook              as Fb
import           Paths_howl_backend

howlSwagger = toSwagger api
  & info.title   .~ "Howl Server API"
  & info.(Data.Swagger.version) .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & schemes ?~ [Https]

spec :: Spec
spec = describe "Swagger (not implemented)" $ do
  it "returns a positive number when given a negative number" $
    1 == 1
  -- context "ToJSON matches ToSchema" $ validateEveryToJSON api
  -- it "swagger.json is up-to-date" $ do
  --   path <- getDataFileName "swagger-generated.json"
  --   swag <- eitherDecode <$> BL8.readFile path
  --   swag `shouldBe` Right howlSwagger

instance Arbitrary Fb.UserAccessToken where
  arbitrary = Fb.UserAccessToken <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary User where
  arbitrary = User <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Fb.Id where
  arbitrary = Fb.Id <$> arbitrary

instance Arbitrary Event where
  arbitrary = Event <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
