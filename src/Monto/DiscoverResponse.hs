{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Monto.DiscoverResponse where

import           Data.Aeson.TH
import           Data.Text (Text)
import qualified Data.Text as T

import           Monto.Types

data DiscoverResponse =
  DiscoverResponse
    { serviceID   :: ServiceID
    , label       :: Text
    , description :: Text
    , language    :: Language
    , product     :: Product
    , options     :: Maybe String
    } deriving (Eq)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "serviceID" -> "service_id"
    label' -> label'
}) ''DiscoverResponse)

instance Show DiscoverResponse where
  show (DiscoverResponse s l d lg p _) =
    concat [ "{", T.unpack s
           , ",", T.unpack l
           , ",", T.unpack d
           , ",", T.unpack lg
           , ",", T.unpack p
           , "}"
           ]