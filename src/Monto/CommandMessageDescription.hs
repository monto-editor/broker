{-# LANGUAGE TemplateHaskell #-}
module Monto.CommandMessageDescription where

import           Data.Aeson.TH
import           Data.Text     (Text)

import           Monto.Types

data CommandMessageDescription = CommandMessageDescription
    { commandMessageTag :: Text
    , language          :: Language
    } deriving (Eq,Show)
$(deriveJSON defaultOptions ''CommandMessageDescription)
