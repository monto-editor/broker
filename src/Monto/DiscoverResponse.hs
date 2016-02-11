{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Monto.DiscoverResponse where

import           Data.Aeson.TH
import           Data.Aeson (Value)
import           Data.Text (Text)

import           Monto.Types
import           Monto.ProductDescription

data DiscoverResponse =
  DiscoverResponse
    { serviceID   :: ServiceID
    , label       :: Text
    , description :: Text
    , products    :: [ProductDescription]
    , options     :: Maybe Value
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "serviceID" -> "service_id"
    label' -> label'
}) ''DiscoverResponse)
