{-# LANGUAGE TemplateHaskell #-}
module Monto.ProductMessage where

import           Data.Aeson        (Value)
import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH
import           Prelude           hiding (id)

import           Monto.Source
import           Monto.Types

data ProductMessage =
  ProductMessage
    { id        :: VersionID
    , source    :: Source
    , serviceID :: ServiceID
    , product   :: Product
    , language  :: Language
    , contents  :: Value
    , time      :: Value
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = snakeCase
}) ''ProductMessage)


instance Ord ProductMessage where
  compare p1 p2 = compare (id p1, source p1, language p1, serviceID p1)
                          (id p2, source p2, language p2, serviceID p2)
