{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Monto.DiscoverResponse where

import           Data.Aeson.TH
import           Data.Aeson (Value)
import           Data.Text (Text)

import           Monto.Types

data DiscoverResponse =
  DiscoverResponse
    { serviceID   :: ServiceID
    , label       :: Text
    , description :: Text
    , language    :: Language
    , product     :: Product
    , options     :: Maybe Value
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "serviceID" -> "service_id"
    label' -> label'
}) ''DiscoverResponse)
