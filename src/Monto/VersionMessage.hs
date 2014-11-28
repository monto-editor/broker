{-# LANGUAGE TemplateHaskell #-}
module Monto.VersionMessage where

import Prelude hiding (id)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Aeson.TH
import Data.Hashable
import Data.Function (on)

data Selection = Selection { begin :: Int, end :: Int }
$(deriveJSON defaultOptions ''Selection)

type Source = Text
type Language = Text

data VersionMessage =
  VersionMessage
    { id :: Int
    , source :: Source
    , language :: Language
    , contents :: Text
    , selection :: Vector Selection
    }
$(deriveJSON defaultOptions ''VersionMessage)
