{-# LANGUAGE TemplateHaskell #-}
module Monto.MessagesIDE where

import           Data.Aeson.TH
import           Monto.CommandMessage
import           Monto.CommandUpdate
import           Monto.ConfigurationMessage
import           Monto.DiscoverRequest
import           Monto.DiscoverResponse
import           Monto.ProductMessage
import           Monto.SourceMessage

data MessageFromIDE = SourceMessage SourceMessage
                    | ConfigurationMessages [ConfigurationMessage]
                    | DiscoverRequest DiscoverRequest
                    | CommandMessage CommandMessage
$(deriveJSON defaultOptions
                 { constructorTagModifier = \con -> case con of
                     "SourceMessage" -> "source"
                     "ConfigurationMessages" -> "configurations"
                     "DiscoverRequest" -> "discovery"
                     "CommandMessage" -> "commandMessage"
                     c -> c
                 } ''MessageFromIDE)

data MessageToIDE = ProductMessage ProductMessage
                  | DiscoverResponse [DiscoverResponse]
                  | CommandUpdate CommandUpdate
$(deriveJSON defaultOptions
                 { constructorTagModifier = \con -> case con of
                     "ProductMessage" -> "product"
                     "DiscoverResponse" -> "discovery"
                     "CommandUpdate" -> "commandUpdate"
                     c -> c
                 } ''MessageToIDE)
