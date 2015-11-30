{-# LANGUAGE OverloadedStrings #-}
module Monto.ServiceDependency where

import Data.Aeson
import Monto.Types
import qualified Data.HashMap.Strict as M

data ServiceDependency = ServiceDependency ServiceID | SourceDependency Language
  deriving (Eq,Ord,Show)

instance FromJSON ServiceDependency where
  parseJSON = withObject "Expected an JSON object while parsing ServiceDependency" $ \obj ->
    if M.member "service_id" obj
    then
      ServiceDependency <$> obj .: "service_id"
    else
      SourceDependency <$> obj .: "source_language"

instance ToJSON ServiceDependency where
  toJSON (ServiceDependency serviceID) = object [ "service_id" .= serviceID ]
  toJSON (SourceDependency lang) = object [ "source_language" .= lang ]
    
