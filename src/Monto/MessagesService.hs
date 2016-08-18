{-# LANGUAGE TemplateHaskell #-}
module Monto.MessagesService where

import           Data.Aeson.TH
import           Monto.CommandMessage
import           Monto.ConfigurationMessage
import           Monto.ProductMessage
import           Monto.RegisterCommandMessageDependencies
import           Monto.RegisterDynamicDependencies
import           Monto.Request

data MessageToService = Request Request
                      | ConfigurationMessage ConfigurationMessage
                      | CommandMessage CommandMessage
$(deriveJSON defaultOptions
                 { constructorTagModifier = \con -> case con of
                     "Request" -> "request"
                     "ConfigurationMessage" -> "configuration"
                     "CommandMessage" -> "commandMessage"
                     c -> c
                 } ''MessageToService)

data MessageFromService = ProductMessage ProductMessage
                        | DynamicDependency RegisterDynamicDependencies
                        | CommandMessageDependency RegisterCommandMessageDependencies
$(deriveJSON defaultOptions
                 { constructorTagModifier = \con -> case con of
                     "ProductMessage" -> "product"
                     "DynamicDependency" -> "dynDep"
                     "CommandMessageDependency" -> "cmdDep"
                     c -> c
                 } ''MessageFromService)
