{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Monto.RegisterServiceRequest where

import           Data.Aeson.TH
import           Data.Aeson (Value)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe (fromMaybe)

import           Monto.Types

data RegisterServiceRequest =
  RegisterServiceRequest
    { serviceID    :: ServiceID
    , label        :: Text
    , description  :: Text
    , language     :: Language
    , product      :: Product
    , options      :: Maybe Value
    , dependencies :: Maybe (Vector String)
    } deriving (Eq)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "serviceID" -> "service_id"
    label' -> label'
}) ''RegisterServiceRequest)

registerServiceRequestDependencies :: RegisterServiceRequest -> Vector String
registerServiceRequestDependencies = fromMaybe V.empty . dependencies

instance Show RegisterServiceRequest where
  show (RegisterServiceRequest i _ _ l p _ _) =
    concat [ "{", T.unpack i
           , ",", T.unpack l
           , ",", T.unpack p
           , "}"
           ]