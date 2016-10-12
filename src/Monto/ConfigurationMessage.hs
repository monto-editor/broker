{-# LANGUAGE TemplateHaskell #-}
module Monto.ConfigurationMessage where

import           Data.Aeson        (Value)
import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH

import           Monto.Types

data ConfigurationMessage =
  ConfigurationMessage
    { serviceID :: ServiceID
    , settings  :: Value
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = snakeCase
}) ''ConfigurationMessage)
