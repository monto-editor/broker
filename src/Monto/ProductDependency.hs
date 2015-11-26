{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Monto.ProductDependency where

import           Data.Aeson
import           Data.Text (Text,unpack)
import           Monto.Types

data ProductDependency
  = Version (VersionID,Source,Language)
  | Product (VersionID,Source,Language,Product)
  deriving (Eq,Ord,Show)

instance ToJSON ProductDependency where
  toJSON (Version (vid,s,l)) = object
    [ "tag"        .= ("version" :: Text)
    , "version_id" .= vid
    , "source"     .= s
    , "language"   .= l
    ]
  toJSON (Product (vid,s,l,p)) = object
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
      "version" -> do
        return $ Version (vid,s,l)
      "product" -> do
        p   <- obj .: "product"
        return $ Product (vid,s,l,p)
      _ -> fail "tag has to be version or product"

type Invalid = ProductDependency
type ReverseProductDependency = ProductDependency
