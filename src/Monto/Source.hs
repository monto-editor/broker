{-# LANGUAGE TemplateHaskell #-}
module Monto.Source where

import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH
import           Data.Text         (Text)

data Source =
  Source
    { physicalName :: Text,
      logicalName  :: Maybe Text
    } deriving (Show)

instance Ord Source where
  compare x y = compare (physicalName x) (physicalName y)

instance Eq Source where
  x == y = physicalName x == physicalName y

$(deriveJSON (defaultOptions {
  fieldLabelModifier = snakeCase
}) ''Source)
