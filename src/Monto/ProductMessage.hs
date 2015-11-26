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
    , source       :: Source
    , serviceId    :: ServiceID
    , product      :: Product
    , language     :: Language
    , contents     :: Value
    , dependencies :: Maybe (Vector ProductDependency)
    } deriving (Eq)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "versionId" -> "version_id"
    "serviceId" -> "service_id"
    label -> label
}) ''ProductMessage)

productDependencies :: ProductMessage -> Vector ProductDependency
productDependencies = fromMaybe V.empty . dependencies

instance Show ProductMessage where
  show (ProductMessage i s sid p l _ _) =
    concat [ "{", show i
           , ",", T.unpack s
           , ",", T.unpack sid
           , ",", T.unpack p
           , ",", T.unpack l
           , "}"
           ]
