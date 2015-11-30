{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Monto.ConfigurationMessage where

import           Data.Aeson.TH
import           Data.Aeson (Value)

import           Monto.Types

data ServiceConfiguration =
  ServiceConfiguration
    { serviceID      :: ServiceID
    , configurations :: Value
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "serviceID" -> "service_id"
    label' -> label'
}) ''ServiceConfiguration)

data ConfigurationMessage =
  ConfigurationMessage
    { configureServices :: [ServiceConfiguration]
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "configureServices" -> "configure_services"
    label' -> label'
}) ''ConfigurationMessage)
