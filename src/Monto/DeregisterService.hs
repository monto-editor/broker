{-# LANGUAGE TemplateHaskell #-}
module Monto.DeregisterService where

import           Data.Aeson.TH
import           Data.Aeson.Casing (snakeCase)

import           Monto.Types

data DeregisterService =
  DeregisterService
    { deregisterServiceID   :: ServiceID
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = snakeCase
}) ''DeregisterService)
