{-# LANGUAGE TemplateHaskell #-}
module Monto.ProductMessage where

import           Prelude hiding (id)
import           Data.Aeson.TH
import           Data.Aeson (Value)

import           Monto.Types

data ProductMessage =
  ProductMessage
    { id           :: VersionID
    , source       :: Source
    , serviceID    :: ServiceID
    , product      :: Product
    , language     :: Language
    , contents     :: Value
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "serviceID" -> "service_id"
    label -> label
}) ''ProductMessage)


instance Ord ProductMessage where
  compare p1 p2 = compare (id p1, source p1, language p1, serviceID p1)
                          (id p2, source p2, language p2, serviceID p2)
