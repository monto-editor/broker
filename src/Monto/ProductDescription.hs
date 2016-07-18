{-# LANGUAGE TemplateHaskell #-}
module Monto.ProductDescription where

import           Data.Aeson.TH

import           Monto.Types

data ProductDescription = ProductDescription
    { product  :: Product
    , language :: Language
    } deriving (Eq,Show)
$(deriveJSON defaultOptions ''ProductDescription)
