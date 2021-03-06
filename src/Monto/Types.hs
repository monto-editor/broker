{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Monto.Types where

import           Data.Aeson.TH
import           Data.String
import           Data.Text     (Text)

newtype VersionID = VersionID Int
  deriving (Eq,Ord,Show)
$(deriveJSON defaultOptions ''VersionID)

newtype Language = Language Text
  deriving (Eq,Ord,Show,IsString)
$(deriveJSON defaultOptions ''Language)

newtype Product = Product Text
  deriving (Eq,Ord,Show,IsString)
$(deriveJSON defaultOptions ''Product)

newtype Command = Command Text
  deriving (Eq,Ord,Show,IsString)
$(deriveJSON defaultOptions ''Command)

newtype Port = Port Int
  deriving (Eq,Ord,Show,Read,Enum)
$(deriveJSON defaultOptions ''Port)

newtype ServiceID = ServiceID Text
  deriving (Eq,Ord,Show,IsString)
$(deriveJSON defaultOptions ''ServiceID)

newtype OptionID = OptionID Text
  deriving (Eq,Ord,Show,IsString)
$(deriveJSON defaultOptions ''OptionID)

class IsText a where
  toText   :: a -> Text
  fromText :: Text -> a

instance IsText Language where
  toText (Language l) = l
  fromText = Language

instance IsText Product where
  toText (Product p) = p
  fromText = Product

instance IsText ServiceID where
  toText (ServiceID s) = s
  fromText = ServiceID

instance IsText Command where
  toText (Command c) = c
  fromText = Command
