{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Monto.RegisterServiceRequest where

import           Data.Aeson               (Value)
import           Data.Aeson.Casing        (snakeCase)
import           Data.Aeson.TH
import           Data.Text                (Text)

import           Monto.ProductDependency
import           Monto.ProductDescription
import           Monto.Types

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
  fieldLabelModifier = snakeCase
}) ''RegisterServiceRequest)
