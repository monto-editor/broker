{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Monto.DynamicDependency where

import           Data.Aeson.TH
import qualified Monto.Edge as E
import           Monto.Types

toGraphTuple :: DynamicDependency -> ([(Product, Language)], (Source, ServiceID))
toGraphTuple dyndep =
    let edges' = flip map (edges dyndep) $ \e ->
                 (E.product e, E.language e)
    in (edges', (source dyndep, serviceID dyndep))

data DynamicDependency = DynamicDependency
    { source :: Source
    , serviceID :: ServiceID
    , edges :: [E.Edge]
    } deriving (Eq,Show,Ord)
$(deriveJSON (defaultOptions {
  fieldLabelModifier = \s -> case s of
    "serviceID" -> "service_id"
    label -> label
}) ''DynamicDependency)
