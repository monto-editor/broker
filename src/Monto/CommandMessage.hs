{-# LANGUAGE TemplateHaskell #-}
module Monto.CommandMessage where

import           Prelude           hiding (id)

import           Data.Aeson        (Value)
import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH
import           Data.Text         (Text)

import           Monto.Request     (Message)
import           Monto.Types

data CommandMessage =
  CommandMessage
    { session      :: Int
    , id           :: Int
    , command      :: Command
    , language     :: Language
    , contents     :: Value
    , requirements :: [Message]
    } deriving (Show)

instance Ord CommandMessage where
  compare x y = compare (session x, id x, command x) (session y, id y, command y)

instance Eq CommandMessage where
  x == y = (session x, id x, command x) == (session y, id y, command y)

$(deriveJSON (defaultOptions {
  fieldLabelModifier = snakeCase
}) ''CommandMessage)
