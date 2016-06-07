{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Monto.ConfigurationMessage where

import           Data.Aeson.TH
import           Data.Aeson (Value)

import           Monto.Types

data ConfigurationMessage =
  ConfigurationMessage
    { serviceID      :: ServiceID
    , configurations :: Value
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "serviceID" -> "service_id"
    label' -> label'
}) ''ConfigurationMessage)
