{-# LANGUAGE TemplateHaskell #-}
module Monto.RegisterServiceResponse where

import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Monto.Types

data RegisterServiceResponse =
  RegisterServiceResponse
    { response   :: Text
    , bindOnPort :: Maybe Port
    } deriving (Eq)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = snakeCase
}) ''RegisterServiceResponse)

instance Show RegisterServiceResponse where
  show (RegisterServiceResponse l p) =
    concat [ "{", T.unpack l
           , ",", show p
           , "}"
           ]
