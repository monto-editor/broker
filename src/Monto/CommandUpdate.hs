{-# LANGUAGE TemplateHaskell #-}
module Monto.CommandUpdate where

import           Prelude           hiding (id)

import           Data.Aeson        (Value)
import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH
import           Data.Text         (Text)

import           Monto.Types

data CommandUpdate =
  CommandUpdate
    { session         :: Int
    , id              :: Int
    , sourceServiceID :: ServiceID
    , tag             :: Text
    , contents        :: Value
    } deriving (Show)

instance Eq CommandUpdate where
  x == y = (session x, id x, sourceServiceID x, tag x) == (session y, id y, sourceServiceID y, tag y)

$(deriveJSON (defaultOptions {
  fieldLabelModifier = snakeCase
}) ''CommandUpdate)
