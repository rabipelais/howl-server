{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators, TypeFamilies#-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Howl
import Howl.Facebook.Types

import Control.Lens
import           Data.Aeson
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Servant.Swagger
import           Data.Swagger

howlSwagger = toSwagger api
  & info.title   .~ "Howl Server API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & schemes ?~ [Https]

writeSwaggerJSON = BL8.writeFile "swagger.json" (encodePretty howlSwagger)

main = writeSwaggerJSON
