{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Monto.DiscoverRequest where

import           Data.Aeson.TH
import           Data.Aeson.Casing (snakeCase)

import           Monto.Types

data ServiceDiscover =
  ServiceDiscover
    { serviceID :: Maybe ServiceID
    , language  :: Maybe Language
    , product   :: Maybe Product
    } deriving (Eq)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = snakeCase
}) ''ServiceDiscover)

instance Show ServiceDiscover where
  show (ServiceDiscover s l p) =
    concat [ "{", show s
           , ",", show l
           , ",", show p
           , "}"
           ]

data DiscoverRequest =
  DiscoverRequest
    { discoverServices :: [ServiceDiscover]
    } deriving (Eq)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = snakeCase
}) ''DiscoverRequest)

instance Show DiscoverRequest where
  show (DiscoverRequest s) =
    concat [ "{", show s
           , "}"
           ]