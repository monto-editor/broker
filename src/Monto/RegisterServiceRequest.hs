{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Monto.RegisterServiceRequest where

import           Data.Aeson.TH
import           Data.Aeson (Value)
import           Data.Text (Text)

import           Monto.Types
import           Monto.ProductDescription
import           Monto.ProductDependency

data RegisterServiceRequest =
  RegisterServiceRequest
    { serviceID    :: ServiceID
    , label        :: Text
    , description  :: Text
    , options      :: Maybe Value
    , products     :: [ProductDescription]
    , dependencies :: [ProductDependency]
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "serviceID" -> "service_id"
    label' -> label'
}) ''RegisterServiceRequest)
