{-# LANGUAGE TemplateHaskell #-}
module Monto.SourceMessage where

import           Prelude hiding (id)
import           Data.Text (Text)
import           Data.Vector (Vector)
import           Data.Aeson.TH

import           Monto.Types

data Selection = Selection { begin :: Int, end :: Int }
  deriving (Eq,Show)
$(deriveJSON defaultOptions ''Selection)

data SourceMessage =
  SourceMessage
    { id         :: VersionID
    , source     :: Source
    , language   :: Language
    , contents   :: Text
    , selections :: Maybe (Vector Selection)
    } deriving (Eq,Show)
$(deriveJSON defaultOptions ''SourceMessage)

instance Ord SourceMessage where
  compare v1 v2 = compare (id v1, source v1, language v1, contents v1)
                          (id v2, source v2, language v2, contents v2)
