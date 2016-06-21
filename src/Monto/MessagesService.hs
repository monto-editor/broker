{-# LANGUAGE TemplateHaskell #-}
module Monto.MessagesService where

import           Data.Aeson.TH
import           Monto.Request
import           Monto.ConfigurationMessage
import           Monto.ProductMessage
import           Monto.RegisterDynamicDependencies

data MessageToService = Request Request
                      | ConfigurationMessage ConfigurationMessage
$(deriveJSON defaultOptions
                 { constructorTagModifier = \con -> case con of
                     "Request" -> "request"
                     "ConfigurationMessage" -> "configuration"
                     c -> c
                 } ''MessageToService)

data MessageFromService = ProductMessage ProductMessage
                        | DynamicDependency RegisterDynamicDependencies
$(deriveJSON defaultOptions
                 { constructorTagModifier = \con -> case con of
                     "ProductMessage" -> "product"
                     "DynamicDependency" -> "dependency"
                     c -> c
                 } ''MessageFromService)
