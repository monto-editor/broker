{-# LANGUAGE OverloadedStrings #-}
module Monto.ProductDependency where

import           Data.Aeson
import           Monto.Types
import qualified Data.HashMap.Strict as M

data ProductDependency
    = ProductDependency
      { serviceID :: ServiceID
      , product :: Product
      , language :: Language
      }
    | SourceDependency
      { language :: Language
      }
    deriving (Eq,Show)

instance ToJSON ProductDependency where
  toJSON (SourceDependency l) = object
    [ "language" .= l
    ]
  toJSON (ProductDependency s p l) = object
    [ "service_id" .= s
    , "product" .= p
    , "language" .= l
    ]

instance FromJSON ProductDependency where
  parseJSON = withObject "ProductDependency" $ \obj -> do
    lang <- obj .: "language"
    if M.member "service_id" obj
    then do
      sid <- obj .: "service_id"
      prod <- obj .: "product"
      return $ ProductDependency sid prod lang
    else return $ SourceDependency lang
