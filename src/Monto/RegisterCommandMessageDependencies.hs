{-# LANGUAGE TemplateHaskell #-}
module Monto.RegisterCommandMessageDependencies where

import           Data.Aeson.Casing    (snakeCase)
import           Data.Aeson.TH

import           Monto.CommandMessage
import           Monto.Types

data RegisterCommandMessageDependencies = RegisterCommandMessageDependencies
    { commandMessage :: CommandMessage
    , dependencies   :: [(Source,ServiceID,Product,Language)]
    } deriving (Eq,Show,Ord)

$(deriveJSON (defaultOptions {
  fieldLabelModifier = snakeCase
}) ''RegisterCommandMessageDependencies)
