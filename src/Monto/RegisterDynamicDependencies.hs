{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Monto.RegisterDynamicDependencies where

import           Data.Aeson.TH
import           Data.Aeson.Casing (snakeCase)
import           Monto.DynamicDependency
import           Monto.Types

data RegisterDynamicDependencies = RegisterDynamicDependencies
    { source :: Source
    , serviceID :: ServiceID
    , dependencies :: [DynamicDependency]
    } deriving (Eq,Show,Ord)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = snakeCase
}) ''RegisterDynamicDependencies)
