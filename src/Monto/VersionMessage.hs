{-# LANGUAGE TemplateHaskell #-}
module Monto.VersionMessage where

import           Prelude hiding (id)
import           Data.Text (Text)
import           Data.Vector (Vector)
import           Data.Aeson.TH

import           Monto.Types

data Selection = Selection { begin :: Int, end :: Int }
  deriving (Eq,Show)
$(deriveJSON defaultOptions ''Selection)

data VersionMessage =
  VersionMessage
    { versionId  :: VersionID
    , source     :: Source
    , language   :: Language
    , contents   :: Text
    , selections :: Maybe (Vector Selection)
    } deriving (Eq,Show)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "versionId" -> "version_id"
    label -> label
}) ''VersionMessage)
