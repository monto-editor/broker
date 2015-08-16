{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Monto.DiscoverResponse where

import           Data.Aeson.TH
import           Data.Vector (Vector)

import           Monto.Types

data DiscoverResponse =
  DiscoverResponse
    { serviceID :: ServiceID
    , language  :: Language
    , product   :: Product
    , status    :: String
    } deriving (Eq)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "serviceID" -> "service_id"
    label -> label
}) ''DiscoverResponse)

instance Show DiscoverResponse where
  show (DiscoverResponse s l p st) =
    concat [ "{", T.unpack s
           , ",", T.unpack l
           , ",", T.unpack p
           , ",", st
           , "}"
           ]