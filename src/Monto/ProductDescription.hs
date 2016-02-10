{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Monto.ProductDescription where

import           Data.Aeson.TH

import           Monto.Types
import           Monto.ServiceDependency

data ProductDescription = ProductDescription
    { product :: Product
    , dependsOn :: [ServiceDependency]
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "serviceID" -> "service_id"
    label' -> label'
}) ''ProductDescription)
