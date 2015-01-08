module Monto.ServerDependency where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Char (isAlphaNum)

data ServerDependency = Server { name :: Text } | Source | Star
  deriving (Eq,Ord)
type Server = ServerDependency

instance Show ServerDependency where
  show (Server n) = T.unpack n
  show Source = "Source"
  show Star = "*"

instance Read ServerDependency where
  readsPrec _ "" = []
  readsPrec _ r =
    let (s,r') = break (not . isAlphaNum) r
    in [(Server (T.pack s), r')]
