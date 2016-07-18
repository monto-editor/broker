{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Monto.DiscoverResponse where

import           Data.Aeson               (Value)
import           Data.Aeson.Casing        (snakeCase)
import           Data.Aeson.TH
import           Data.Text                (Text)

import           Monto.ProductDescription
import           Monto.Types

data DiscoverResponse =
  DiscoverResponse
    { serviceID   :: ServiceID
    , label       :: Text
    , description :: Text
    , products    :: [ProductDescription]
    , options     :: Maybe Value
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = snakeCase
}) ''DiscoverResponse)
