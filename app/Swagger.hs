{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Howl
import           Howl.Facebook.Types

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Swagger
import           Servant.RawM               (RawM)
import           Servant.Swagger

instance HasSwagger RawM where
  toSwagger _ = mempty & paths . at "/" ?~ mempty

howlSwagger = toSwagger api
  & info.title   .~ "Howl Server API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & schemes ?~ [Https]

writeSwaggerJSON = BL8.writeFile "swagger-generated.json" (encodePretty howlSwagger)

main = writeSwaggerJSON
