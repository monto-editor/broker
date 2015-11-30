{-# LANGUAGE TemplateHaskell #-}
module Monto.DeregisterService where

import           Data.Aeson.TH

import           Monto.Types

data DeregisterService =
  DeregisterService
    { deregisterServiceID   :: ServiceID
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "deregisterServiceID" -> "deregister_service_id"
    label -> label
}) ''DeregisterService)
