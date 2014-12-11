{-# LANGUAGE TemplateHaskell #-}
module Monto.Dependency where

import           Data.Aeson.TH
import           Data.Text (Text)

type VersionID = Int
type ProductID = Int
type Source    = Text
type Language  = Text
type Product   = Text

data Dependency
  = Version (VersionID,Source,Language)
  | Product (VersionID,ProductID,Source,Language,Product)
  deriving (Eq,Ord,Show)
$(deriveJSON defaultOptions ''Dependency)

type Invalid = Dependency
type ReverseDependency = Dependency
