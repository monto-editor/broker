{-# LANGUAGE TemplateHaskell #-}
module Monto.CommandMessage where

import           Data.Aeson.TH
import           Data.Aeson (Value)
import           Data.Aeson.Casing (snakeCase)
import           Data.Text (Text)

import           Monto.Types

data CommandMessage =
  CommandMessage
    { serviceID  :: ServiceID
    , tag        :: Text
    , contents   :: Value
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = snakeCase
}) ''CommandMessage)
