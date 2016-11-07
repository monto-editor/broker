{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Monto.DynamicDependency where

import           Prelude           hiding (product)

import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH
import           Data.Text         (Text)
import qualified Data.Text         as T

import           Monto.Source
import           Monto.Types

data DynamicDependency = DynamicDependency
    { source    :: Source
    , serviceID :: ServiceID
    , product   :: Product
    , language  :: Language
    } deriving (Eq,Show,Ord)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = snakeCase
}) ''DynamicDependency)

toPrintableText :: [DynamicDependency] -> Text
toPrintableText deps = T.intercalate ", " $ map (\dynDep -> T.concat ["{", physicalName $ source dynDep, " ", toText $ serviceID dynDep, " ", toText $ product dynDep, " ", toText $ language dynDep, "}"]) deps
