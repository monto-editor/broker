{-# Language TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Monto.Types where

import           Data.Aeson.TH
import           Data.Text (Text)
import           Data.String

newtype VersionID = VersionID Int
  deriving (Eq,Ord,Show)
$(deriveJSON defaultOptions ''VersionID)

newtype Source    = Source Text
  deriving (Eq,Ord,Show,IsString)
$(deriveJSON defaultOptions ''Source)

newtype Language = Language Text
  deriving (Eq,Ord,Show,IsString)
$(deriveJSON defaultOptions ''Language)

newtype Product = Product Text
  deriving (Eq,Ord,Show,IsString)
$(deriveJSON defaultOptions ''Product)

newtype Port = Port Int
  deriving (Eq,Ord,Show,Read,Enum)
$(deriveJSON defaultOptions ''Port)

newtype ServiceID = ServiceID Text
  deriving (Eq,Ord,Show,IsString)
$(deriveJSON defaultOptions ''ServiceID)

class IsText a where
  toText   :: a -> Text
  fromText :: Text -> a

instance IsText Source where
  toText (Source s) = s
  fromText = Source

instance IsText Language where
  toText (Language l) = l
  fromText = Language

instance IsText Product where
  toText (Product p) = p
  fromText = Product

instance IsText ServiceID where
  toText (ServiceID s) = s
  fromText = ServiceID
