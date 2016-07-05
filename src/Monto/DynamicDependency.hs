{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Monto.DynamicDependency where

import           Data.Aeson.TH
import           Data.Aeson.Casing (snakeCase)
import           Monto.Types

data DynamicDependency = DynamicDependency
    { source :: Source
    , serviceID :: ServiceID
    , product :: Product
    , language :: Language
    } deriving (Eq,Show,Ord)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = snakeCase
}) ''DynamicDependency)
