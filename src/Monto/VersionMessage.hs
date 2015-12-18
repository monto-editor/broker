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


instance Ord VersionMessage where
  compare v1 v2 = compare (versionId v1, source v1, language v1, contents v1)
                          (versionId v2, source v2, language v2, contents v2)
