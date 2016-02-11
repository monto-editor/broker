module Monto.Service where

import           Data.Aeson (Value)
import           Data.Ord
import           Data.Text(Text)

import           Monto.Types
import           Monto.ProductDescription

data Service = Service
  { serviceID   :: ServiceID
  , label       :: Text
  , description :: Text
  , language    :: Language
  , products    :: [ProductDescription]
  , port        :: Port
  , options     :: Maybe Value
  }
  deriving (Eq,Show)

instance Ord Service where
  compare = comparing serviceID
