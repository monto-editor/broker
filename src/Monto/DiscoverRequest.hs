{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Monto.DiscoverRequest where

import           Data.Aeson.TH
import           Data.Vector (Vector)

import           Monto.Types

data DiscoverRequest =
  DiscoverRequest
    { checkServiceIDs :: Vector ServiceID
    , languages       :: Vector Language
    , products        :: Vector Product
    } deriving (Eq)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "checkServiceIDs" -> "check_service_ids"
    label -> label
}) ''DiscoverRequest)

instance Show DiscoverRequest where
  show (DiscoverRequest s l p) =
    concat [ "{", show s
           , ",", show l
           , ",", show p
           , "}"
           ]