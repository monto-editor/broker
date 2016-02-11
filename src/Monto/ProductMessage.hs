{-# LANGUAGE TemplateHaskell #-}
module Monto.ProductMessage where

import           Data.Aeson.TH
import           Data.Aeson (Value)

import           Monto.Types

data ProductMessage =
  ProductMessage
    { versionId    :: VersionID
    , source       :: Source
    , serviceId    :: ServiceID
    , product      :: Product
    , language     :: Language
    , contents     :: Value
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "versionId" -> "version_id"
    "serviceId" -> "service_id"
    label -> label
}) ''ProductMessage)


instance Ord ProductMessage where
  compare p1 p2 = compare (versionId p1, source p1, language p1, serviceId p1)
                          (versionId p2, source p2, language p2, serviceId p2)
