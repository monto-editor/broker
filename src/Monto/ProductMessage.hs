{-# LANGUAGE TemplateHaskell #-}
module Monto.ProductMessage where

import           Data.Aeson.TH
import           Data.Aeson (Value)

import           Monto.Types

data ProductMessage =
  ProductMessage
    { versionID    :: VersionID
    , source       :: Source
    , serviceID    :: ServiceID
    , product      :: Product
    , language     :: Language
    , contents     :: Value
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "versionID" -> "version_id"
    "serviceID" -> "service_id"
    label -> label
}) ''ProductMessage)


instance Ord ProductMessage where
  compare p1 p2 = compare (versionID p1, source p1, language p1, serviceID p1)
                          (versionID p2, source p2, language p2, serviceID p2)
