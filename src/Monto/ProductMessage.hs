{-# LANGUAGE TemplateHaskell #-}
module Monto.ProductMessage where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Aeson.TH
import Monto.VersionMessage(Source,Language)

type Product = Text

data ProductMessage =
  ProductMessage
    { id :: Int
    , source :: Source
    , product :: Product
    , language :: Language
    , contents :: Text
    }
$(deriveJSON defaultOptions ''ProductMessage)
