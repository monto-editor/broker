{-# LANGUAGE TemplateHaskell #-}
module Monto.MessagesService where

import           Data.Aeson.TH
import           Monto.ConfigurationMessage
import           Monto.CommandMessage
import           Monto.ProductMessage
import           Monto.RegisterDynamicDependencies
import           Monto.Request

data MessageToService = Request Request
                      | ConfigurationMessage ConfigurationMessage
                      | CommandMessage CommandMessage
$(deriveJSON defaultOptions
                 { constructorTagModifier = \con -> case con of
                     "Request" -> "request"
                     "ConfigurationMessage" -> "configuration"
                     "CommandMessage" -> "command"
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
