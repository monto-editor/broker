{-# LANGUAGE TemplateHaskell #-}
module Monto.IDEMessages where

import           Data.Aeson.TH
import           Monto.SourceMessage
import           Monto.ProductMessage
import           Monto.ConfigurationMessage
import           Monto.DiscoverRequest
import           Monto.DiscoverResponse

data IDEReceive = SourceMessage SourceMessage
                | ConfigurationMessages [ConfigurationMessage]
                | DiscoverRequest DiscoverRequest
$(deriveJSON defaultOptions
                 { constructorTagModifier = \con -> case con of
                     "SourceMessage" -> "source"
                     "ConfigurationMessages" -> "configuration"
                     "DiscoverRequest" -> "discovery"
                     c -> c
                 } ''IDEReceive)

data IDESend = ProductMessage ProductMessage
             | DiscoverResponse [DiscoverResponse]
$(deriveJSON defaultOptions
                 { constructorTagModifier = \con -> case con of
                     "ProductMessage" -> "product"
                     "DiscoverResponse" -> "discovery"
                     c -> c
                 } ''IDESend)
