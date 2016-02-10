{-# LANGUAGE OverloadedStrings #-}
module Monto.ProductDependency where

import           Data.Aeson
import           Monto.Types
import qualified Data.HashMap.Strict as M

data ProductDependency
  = VersionDependency Source Language
  | ProductDependency Source ServiceID Product Language
  deriving (Eq,Show)

instance ToJSON ProductDependency where
  toJSON (VersionDependency s l) = object
    [ "source"     .= s
    , "language"   .= l
    ]
  toJSON (ProductDependency s sid prod lang) = object
    [ "source"     .= s
    , "service_id" .= sid
    , "product" .= prod
    , "language" .= lang
    ]

instance FromJSON ProductDependency where
  parseJSON = withObject "ProductDependency" $ \obj -> do
    s <- obj .: "source"
    lang <- obj .: "language"
    if M.member "service_id" obj
    then do
      sid <- obj .: "service_id"
      prod <- obj .: "product"
      return $ ProductDependency s sid prod lang
    else return $ VersionDependency s lang

type Invalid = ProductDependency
type ReverseProductDependency = ProductDependency
