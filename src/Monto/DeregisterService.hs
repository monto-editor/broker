{-# LANGUAGE TemplateHaskell #-}
module Monto.DeregisterService where

import           Data.Aeson.TH
import qualified Data.Text as T

import           Monto.Types

data DeregisterService =
  DeregisterService
    { deregisterServiceID   :: ServiceID
    } deriving (Eq)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "deregisterServiceID" -> "deregister_service_id"
    label -> label
}) ''DeregisterService)

instance Show DeregisterService where
  show (DeregisterService i) =
    concat [ "{", T.unpack i
           , "}"
           ]
