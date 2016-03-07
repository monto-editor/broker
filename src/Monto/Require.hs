{-# LANGUAGE TemplateHaskell #-}
module Monto.Require where

import           Prelude hiding (id)
import           Data.Aeson.TH

import           Monto.Types

data Require =
  Require
    { requiredSources :: [Source]
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "requiredSources" -> "required_sources"
    label -> label
}) ''Require)