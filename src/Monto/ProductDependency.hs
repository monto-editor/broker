{-# LANGUAGE OverloadedStrings #-}
module Monto.ProductDependency where

import           Data.Aeson
import           Monto.Types
import qualified Data.HashMap.Strict as M

data ProductDependency
  = VersionDependency VersionID Source Language
  | ProductDependency VersionID Source ServiceID
  deriving (Eq,Show)

instance ToJSON ProductDependency where
  toJSON (VersionDependency vid s l) = object
    [ "version_id" .= vid
    , "source"     .= s
    , "language"   .= l
    ]
  toJSON (ProductDependency vid s sid) = object
    [ "version_id" .= vid
    , "source"     .= s
    , "service_id" .= sid
    ]

instance FromJSON ProductDependency where
  parseJSON = withObject "ProductDependency" $ \obj -> do
    vid <- obj .: "version_id"
    s   <- obj .: "source"
    if M.member "service_id" obj
    then do
      sid <- obj .: "service_id"
      return $ ProductDependency vid s sid
    else do
      l <- obj .: "language"
      return $ VersionDependency vid s l

type Invalid = ProductDependency
type ReverseProductDependency = ProductDependency
