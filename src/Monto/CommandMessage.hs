{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Monto.CommandMessage where

import           Prelude           hiding (id)

import           Data.Aeson        (Value)
import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH
import           Data.Text         (Text)
import qualified Data.Text         as T

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
  compare x y = compare (session x, id x, command x, language x) (session y, id y, command y, language y)

instance Eq CommandMessage where
  x == y = (session x, id x, command x, language x) == (session y, id y, command y, language y)

$(deriveJSON (defaultOptions {
  fieldLabelModifier = snakeCase
}) ''CommandMessage)

toPrintableText :: CommandMessage -> Text
toPrintableText cmdMsg = T.concat ["cmdMsg {", T.pack $ show $ session cmdMsg, ":", T.pack $ show $ id cmdMsg, " ", toText $ command cmdMsg, " ", toText $ language cmdMsg, "}"]
