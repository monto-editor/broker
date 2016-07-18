{-# LANGUAGE TemplateHaskell #-}
module Monto.DeregisterService where

import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH

import           Monto.Types

data DeregisterService =
  DeregisterService
    { deregisterServiceID :: ServiceID
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = snakeCase
}) ''DeregisterService)
