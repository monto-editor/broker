{-# LANGUAGE TemplateHaskell #-}
module Monto.ServiceMessages where

import           Data.Aeson.TH
import           Monto.Request
import           Monto.ConfigurationMessage
import           Monto.ProductMessage
import           Monto.RegisterDynamicDependencies

data ServiceSend = Request Request
                 | ConfigurationMessage ConfigurationMessage
$(deriveJSON defaultOptions
                 { constructorTagModifier = \con -> case con of
                     "Request" -> "request"
                     "ConfigurationMessage" -> "configuration"
                     c -> c
                 } ''ServiceSend)

data ServiceReceive = ProductMessage ProductMessage
                    | DynamicDependency RegisterDynamicDependencies
$(deriveJSON defaultOptions
                 { constructorTagModifier = \con -> case con of
                     "ProductMessage" -> "product"
                     "DynamicDependency" -> "dependency"
                     c -> c
                 } ''ServiceReceive)
