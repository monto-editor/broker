{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Monto.Dependency where

import           Data.Aeson
import           Data.Text (Text,unpack)

type VersionID = Int
type ProductID = Int
type Source    = Text
type Language  = Text
type Product   = Text

data Dependency
  = Version (VersionID,Source,Language)
  | Product (VersionID,ProductID,Source,Language,Product)
  deriving (Eq,Ord,Show)

instance ToJSON Dependency where
  toJSON (Version (vid,s,l)) = object
    [ "tag"        .= ("version" :: Text)
    , "version_id" .= vid
    , "source"     .= s
    , "language"   .= l
    ]
  toJSON (Product (vid,pid,s,l,p)) = object
    [ "tag"        .= ("product" :: Text)
    , "version_id" .= vid
    , "product_id" .= pid
    , "source"     .= s
    , "language"   .= l
    , "product"    .= p
    ]

instance FromJSON Dependency where
  parseJSON = withObject "Dependency" $ \obj -> do
    tag <- obj .: "tag"
    vid <- obj .: "version_id"
    s   <- obj .: "source"
    l   <- obj .: "language"
    case unpack tag of
      "version" -> do
        return $ Version (vid,s,l)
      "product" -> do
        pid <- obj .: "product_id"
        p   <- obj .: "product"
        return $ Product (vid,pid,s,l,p)
      _ -> fail "tag has to be version or product"
    

type Invalid = Dependency
type ReverseDependency = Dependency
