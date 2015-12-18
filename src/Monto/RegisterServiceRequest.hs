{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Monto.RegisterServiceRequest where

import           Data.Aeson.TH
import           Data.Aeson (Value)
import           Data.Vector (Vector)
import           Data.Text (Text)

import           Monto.Types
import           Monto.ServiceDependency

data RegisterServiceRequest =
  RegisterServiceRequest
    { serviceID    :: ServiceID
    , label        :: Text
    , description  :: Text
    , language     :: Language
    , products     :: Vector Product
    , options      :: Maybe Value
    , dependencies :: Vector ServiceDependency
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "serviceID" -> "service_id"
    label' -> label'
}) ''RegisterServiceRequest)
