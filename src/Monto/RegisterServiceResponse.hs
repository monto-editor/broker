{-# LANGUAGE TemplateHaskell #-}
module Monto.RegisterServiceResponse where

import           Data.Aeson.TH
import           Data.Text (Text)
import qualified Data.Text as T

import           Monto.Types

data RegisterServiceResponse =
  RegisterServiceResponse
    { response    :: Text
    , bindOnPort  :: Maybe Int
    } deriving (Eq)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "bindOnPort" -> "bind_on_port"
    label -> label
}) ''RegisterServiceResponse)

instance Show RegisterServiceResponse where
  show (RegisterServiceResponse l p) =
    concat [ "{", T.unpack l
           , ",", show p
           , "}"
           ]
