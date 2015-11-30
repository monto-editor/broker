{-# LANGUAGE TemplateHaskell #-}
module Monto.ProductMessage where

import           Data.Aeson.TH
import           Data.Aeson (Value)
import           Data.Vector (Vector)

import           Monto.Types
import           Monto.ProductDependency

data ProductMessage =
  ProductMessage
    { versionId    :: VersionID
    , source       :: Source
    , serviceId    :: ServiceID
    , product      :: Product
    , language     :: Language
    , contents     :: Value
    , dependencies :: Vector ProductDependency
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "versionId" -> "version_id"
    "serviceId" -> "service_id"
    label -> label
}) ''ProductMessage)
