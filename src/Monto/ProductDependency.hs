{-# LANGUAGE OverloadedStrings #-}
module Monto.ProductDependency where

import           Data.Aeson
import           Data.Text (Text,unpack)
import           Monto.Types

data ProductDependency
  = VersionDependency VersionID Source Language
  | ProductDependency VersionID Source Language Product
  deriving (Eq,Show)

instance ToJSON ProductDependency where
  toJSON (VersionDependency vid s l) = object
    [ "tag"        .= ("version" :: Text)
    , "version_id" .= vid
    , "source"     .= s
    , "language"   .= l
    ]
  toJSON (ProductDependency vid s l p) = object
    [ "tag"        .= ("product" :: Text)
    , "version_id" .= vid
    , "source"     .= s
    , "language"   .= l
    , "product"    .= p
    ]

instance FromJSON ProductDependency where
  parseJSON = withObject "ProductDependency" $ \obj -> do
    tag <- obj .: "tag"
    vid <- obj .: "version_id"
    s   <- obj .: "source"
    l   <- obj .: "language"
    case unpack tag of
      "version" ->
        return $ VersionDependency vid s l
      "product" -> do
        p   <- obj .: "product"
        return $ ProductDependency vid s l p
      _ -> fail "tag has to be version or product"

type Invalid = ProductDependency
type ReverseProductDependency = ProductDependency
