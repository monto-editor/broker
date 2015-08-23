{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Monto.DiscoverRequest where

import           Data.Aeson.TH
import           Data.Vector (Vector)

import           Monto.Types

data DiscoverRequest =
  DiscoverRequest
    { serviceID :: Maybe ServiceID
    , language  :: Maybe Language
    , product   :: Maybe Product
    } deriving (Eq)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "serviceID" -> "service_id"
    label -> label
}) ''DiscoverRequest)

instance Show DiscoverRequest where
  show (DiscoverRequest s l p) =
    concat [ "{", show s
           , ",", show l
           , ",", show p
           , "}"
           ]