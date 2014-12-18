{-# LANGUAGE TemplateHaskell #-}
module Monto.ProductMessage where

import           Data.Aeson.TH
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe (fromMaybe)

import           Monto.Dependency

data ProductMessage =
  ProductMessage
    { versionId    :: Int
    , productId    :: Int
    , source       :: Source
    , product      :: Product
    , language     :: Language
    , contents     :: Text
    , dependencies :: Maybe (Vector Dependency)
    , invalid      :: Maybe (Vector Invalid)
    } deriving (Eq)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "versionId" -> "version_id"
    "productId" -> "product_id"
    label -> label
}) ''ProductMessage)

productDependencies :: ProductMessage -> Vector Dependency
productDependencies = fromMaybe V.empty . dependencies

instance Show ProductMessage where
  show (ProductMessage i j s p l _ _ _) =
    concat [ "{", show i
           , ",", show j
           , ",", T.unpack s
           , ",", T.unpack p
           , ",", T.unpack l
           , "}"
           ]
