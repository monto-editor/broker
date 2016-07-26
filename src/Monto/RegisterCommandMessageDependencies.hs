{-# LANGUAGE TemplateHaskell #-}
module Monto.RegisterCommandMessageDependencies where

import           Data.Aeson.Casing       (snakeCase)
import           Data.Aeson.TH

import           Monto.CommandMessage
import           Monto.DynamicDependency

data RegisterCommandMessageDependencies = RegisterCommandMessageDependencies
    { commandMessage :: CommandMessage
    , dependencies   :: [DynamicDependency]
    } deriving (Eq,Show,Ord)

$(deriveJSON (defaultOptions {
  fieldLabelModifier = snakeCase
}) ''RegisterCommandMessageDependencies)
