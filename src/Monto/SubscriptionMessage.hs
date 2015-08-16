{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Monto.SubscriptionMessage where

import           Data.Aeson.TH
import           Data.Vector (Vector)

import           Monto.Types

data SubscriptionMessage =
  SubscriptionMessage
    { subscribeServiceIDs   :: Vector ServiceID
    , subscribeLanguages    :: Vector Language
    , subscribeProducts     :: Vector Product
    , unsubscribeServiceIDs :: Vector ServiceID
    , unsubscribeLanguages  :: Vector Language
    , unsubscribeProducts   :: Vector Product
    } deriving (Eq)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "subscribeServiceIDs" -> "subscribe_service_ids"
    "subscribeLanguages" -> "subscribe_languages"
    "subscribeProducts" -> "subscribe_products"
    "unsubscribeServiceIDs" -> "sunubscribe_service_ids"
    "unsubscribeLanguages" -> "unsubscribe_languages"
    "unsubscribeProducts" -> "unsubscribe_products"
    label -> label
}) ''SubscriptionMessage)

instance Show SubscriptionMessage where
  show (SubscriptionMessage ss sl sp us ul up) =
    concat [ "{", show ss
           , ",", show sl
           , ",", show sp
           , ",", show us
           , ",", show ul
           , ",", show up
           , "}"
           ]