{-# LANGUAGE TemplateHaskell #-}
module Monto.VersionMessage where

import           Prelude hiding (id)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)
import           Data.Aeson.TH

import           Monto.Types

data Selection = Selection { begin :: Int, end :: Int }
  deriving Eq
$(deriveJSON defaultOptions ''Selection)

data VersionMessage =
  VersionMessage
    { versionId  :: Int
    , source     :: Source
    , language   :: Language
    , contents   :: Text
    , selections :: Maybe (Vector Selection)
    } deriving Eq
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "versionId" -> "version_id"
    label -> label
}) ''VersionMessage)

instance Show VersionMessage where
  show (VersionMessage i s l _ _) =
    concat [ "{", show i
           , ",", T.unpack s
           , ",", T.unpack l
           , "}"
           ]
