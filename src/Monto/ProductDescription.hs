{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Monto.ProductDescription where

import           Data.Aeson.TH

import           Monto.Types
import           Monto.ServiceDependency

data ProductDescription = ProductDescription
    { product :: Product
    , language :: Language
    , dependsOn :: [ServiceDependency]
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "dependsOn" -> "depends-on"
    label' -> label'
}) ''ProductDescription)
