{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Monto.Edge where

import           Data.Aeson.TH
import           Monto.Types

data Edge = Edge
    { product :: Product
    , language :: Language
    } deriving (Eq,Show,Ord)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> s
}) ''Edge)
