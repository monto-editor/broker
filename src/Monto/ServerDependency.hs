module Monto.ServerDependency where

import           Data.Text (Text)
import qualified Data.Text as T

data ServerDependency = Server { name :: Text } | Source | Star
  deriving (Eq,Ord)
type Server = ServerDependency

instance Show ServerDependency where
  show (Server n) = T.unpack n
  show Source = "Source"
  show Star = "*"
