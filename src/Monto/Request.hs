module Monto.Request where

import Data.Vector (Vector)

import Monto.Types (Source,Language,Product)

newtype Request = Request
  { requirements :: Vector Requirement
  }
  deriving (Show,Eq)

data Requirement
  = Version Source
  | Product (Source,Language,Product)
  deriving (Show,Eq,Ord)

