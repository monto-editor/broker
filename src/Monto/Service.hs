module Monto.Service where

import           Data.Text(Text)
import           Monto.Types
import           Data.Aeson (Value)

data Service = Service
  { serviceID   :: ServiceID
  , label       :: Text
  , description :: Text
  , language    :: Language
  , product     :: Product
  , port        :: Port
  , options     :: Maybe Value
  }
  deriving (Eq,Show)
