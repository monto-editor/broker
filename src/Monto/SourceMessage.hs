{-# LANGUAGE TemplateHaskell #-}
module Monto.SourceMessage where

import           Data.Aeson.TH
import           Data.Text     (Text)
import           Prelude       hiding (id)

import           Monto.Source
import           Monto.Types

data SourceMessage =
  SourceMessage
    { id       :: VersionID
    , source   :: Source
    , language :: Language
    , contents :: Text
    } deriving (Eq,Show)
$(deriveJSON defaultOptions ''SourceMessage)

instance Ord SourceMessage where
  compare v1 v2 = compare (id v1, source v1, language v1, contents v1)
                          (id v2, source v2, language v2, contents v2)
