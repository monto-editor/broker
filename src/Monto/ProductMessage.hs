{-# LANGUAGE TemplateHaskell #-}
module Monto.ProductMessage where

import           Data.Aeson.TH
import           Data.Aeson (Value)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Text as T
import           Data.Maybe (fromMaybe)

import           Monto.Types
import           Monto.ProductDependency

data ProductMessage =
  ProductMessage
    { versionId    :: Int
    , productId    :: Int
    , source       :: Source
    , product      :: Product
    , language     :: Language
    , contents     :: Value
    , dependencies :: Maybe (Vector ProductDependency)
    } deriving (Eq)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "versionId" -> "version_id"
    "productId" -> "product_id"
    label -> label
}) ''ProductMessage)

productDependencies :: ProductMessage -> Vector ProductDependency
productDependencies = fromMaybe V.empty . dependencies

instance Show ProductMessage where
  show (ProductMessage i j s p l _ _) =
    concat [ "{", show i
           , ",", show j
           , ",", T.unpack s
           , ",", T.unpack p
           , ",", T.unpack l
           , "}"
           ]
