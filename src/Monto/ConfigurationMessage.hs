{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Monto.ConfigurationMessage where

import           Data.Aeson.TH
import           Data.Aeson (Value)
import qualified Data.Text as T

import           Monto.Types

data ServiceConfiguration =
  ServiceConfiguration
    { serviceID      :: ServiceID
    , configurations :: Value
    } deriving (Eq)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "serviceID" -> "service_id"
    label' -> label'
}) ''ServiceConfiguration)

instance Show ServiceConfiguration where
  show (ServiceConfiguration i c) =
    concat [ "{", T.unpack i
           , ",", show c
           , "}"
           ]

data ConfigurationMessage =
  ConfigurationMessage
    { configureServices :: [ServiceConfiguration]
    } deriving (Eq)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "configureServices" -> "configure_services"
    label' -> label'
}) ''ConfigurationMessage)

instance Show ConfigurationMessage where
  show (ConfigurationMessage i) =
    concat [ "{", show i
           , "}"
           ]