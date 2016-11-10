{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}

module Models where

import Data.Aeson
import Data.Text

import Control.Applicative

import Database.Persist.TH

type IDType = Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  iD IDType
  firstName Text
  lastName Text
  email String
  UniqueID iD
  deriving Eq Read Show
|]
