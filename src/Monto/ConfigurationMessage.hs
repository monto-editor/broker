{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Monto.ConfigurationMessage where

import           Data.Aeson.TH
import qualified Data.Text as T

import           Monto.Types

data ConfigurationMessage =
  ConfigurationMessage
    { serviceID      :: ServiceID
    , configurations :: String
    } deriving (Eq)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "serviceID" -> "service_id"
    label' -> label'
}) ''ConfigurationMessage)

instance Show ConfigurationMessage where
  show (ConfigurationMessage i c) =
    concat [ "{", T.unpack i
           , ",", c
           , "}"
           ]