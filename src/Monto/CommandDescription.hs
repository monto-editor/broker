{-# LANGUAGE TemplateHaskell #-}
module Monto.CommandDescription where

import           Data.Aeson.TH

import           Monto.Types

data CommandDescription = CommandDescription
    { command  :: Command
    , language :: Language
    } deriving (Eq,Show,Ord)
$(deriveJSON defaultOptions ''CommandDescription)
