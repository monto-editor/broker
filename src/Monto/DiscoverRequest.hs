{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Monto.DiscoverRequest where

import           Data.Aeson.TH

import           Monto.Types

data ServiceDiscover =
  ServiceDiscover
    { serviceID :: Maybe ServiceID
    , language  :: Maybe Language
    , product   :: Maybe Product
    } deriving (Eq)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "serviceID" -> "service_id"
    label -> label
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
  fieldLabelModifier = \s -> case s of
    "discoverServices" -> "discover_services"
    label -> label
}) ''DiscoverRequest)

instance Show DiscoverRequest where
  show (DiscoverRequest s) =
    concat [ "{", show s
           , "}"
           ]