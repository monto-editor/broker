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
    , serviceID    :: ServiceID
    , tag          :: Text
    , contents     :: Value
    , requirements :: [Message]
    } deriving (Show)

-- TODO instances correct?
instance Ord CommandMessage where
  compare x y =
    let sessionOrd = compare (session x) (session y)
    in case sessionOrd of
      EQ -> compare (id x) (id y)
      _ -> sessionOrd

instance Eq CommandMessage where
  x == y = (session x == session y) && (id x == id y)

$(deriveJSON (defaultOptions {
  fieldLabelModifier = snakeCase
}) ''CommandMessage)
