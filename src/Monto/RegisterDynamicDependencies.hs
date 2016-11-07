{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Monto.RegisterDynamicDependencies where

import           Data.Aeson.Casing       (snakeCase)
import           Data.Aeson.TH
import           Data.Text               (Text)
import qualified Data.Text               as T

import           Monto.DynamicDependency (DynamicDependency)
import qualified Monto.DynamicDependency as DynDep
import           Monto.Source
import           Monto.Types

data RegisterDynamicDependencies = RegisterDynamicDependencies
    { source       :: Source
    , serviceID    :: ServiceID
    , dependencies :: [DynamicDependency]
    } deriving (Eq,Show,Ord)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = snakeCase
}) ''RegisterDynamicDependencies)

toPrintableText :: RegisterDynamicDependencies -> Text
toPrintableText reg = T.concat ["{", toText $ serviceID reg, " ", physicalName $ source reg, "} on [", DynDep.toPrintableText $ dependencies reg, "]"]
