{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Monto.RegisterCommandMessageDependencies where

import           Data.Aeson.Casing       (snakeCase)
import           Data.Aeson.TH
import           Data.Text               (Text)
import qualified Data.Text               as T

import           Monto.CommandMessage    (CommandMessage)
import qualified Monto.CommandMessage    as CmdMsg
import           Monto.DynamicDependency (DynamicDependency)
import qualified Monto.DynamicDependency as DynDep

data RegisterCommandMessageDependencies = RegisterCommandMessageDependencies
    { commandMessage :: CommandMessage
    , dependencies   :: [DynamicDependency]
    } deriving (Eq,Show,Ord)

$(deriveJSON (defaultOptions {
  fieldLabelModifier = snakeCase
}) ''RegisterCommandMessageDependencies)

toPrintableText :: RegisterCommandMessageDependencies -> Text
toPrintableText reg = T.concat [CmdMsg.toPrintableText $ commandMessage reg, " on [", DynDep.toPrintableText $ dependencies reg, "]"]
