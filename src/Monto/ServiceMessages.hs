{-# LANGUAGE TemplateHaskell #-}
module Monto.ServiceMessages where

import           Data.Aeson.TH
import           Monto.Request
import           Monto.ConfigurationMessage
import           Monto.DynamicDependency
import           Monto.ProductMessage

data ServiceSend = Request Request
                 | ConfigurationMessage ConfigurationMessage
$(deriveJSON defaultOptions
                 { constructorTagModifier = \con -> case con of
                     "Request" -> "request"
                     "ConfigurationMessage" -> "configuration"
                     c -> c
                 } ''ServiceSend)

data ServiceReceive = ProductMessage ProductMessage
                    | DynamicDependency DynamicDependency
$(deriveJSON defaultOptions
                 { constructorTagModifier = \con -> case con of
                     "ProductMessage" -> "product"
                     "DynamicDependency" -> "dependency"
                     c -> c
                 } ''ServiceReceive)
