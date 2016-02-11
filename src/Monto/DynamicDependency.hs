{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Monto.DynamicDependency where

import           Data.Aeson.TH
import           Monto.Types

data DynamicDependency = DynamicDependency
    { source :: Source
    , serviceID :: ServiceID
    , product :: Product
    , language :: Language
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "serviceID" -> "service_id"
    label -> label
}) ''DynamicDependency)
