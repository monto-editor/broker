{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Monto.RegisterServiceRequest where

import           Data.Aeson.TH
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Text as T
import           Data.Maybe (fromMaybe)

import           Monto.Types

data RegisterServiceRequest =
  RegisterServiceRequest
    { registerServiceID :: ServiceID
    , language          :: Language
    , product           :: Product
    , dependencies      :: Maybe (Vector String)
    } deriving (Eq)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "registerServiceID" -> "register_service_id"
    label -> label
}) ''RegisterServiceRequest)

registerServiceRequestDependencies :: RegisterServiceRequest -> Vector String
registerServiceRequestDependencies = fromMaybe V.empty . dependencies

instance Show RegisterServiceRequest where
  show (RegisterServiceRequest i l p _) =
    concat [ "{", T.unpack i
           , ",", T.unpack l
           , ",", T.unpack p
           , "}"
           ]