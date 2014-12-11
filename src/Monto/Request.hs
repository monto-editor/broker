module Monto.Request where

import Monto.VersionMessage (Source,Language)
import Monto.ProductMessage (Product)
import Monto.MessageStore (MessageStore,versionExists,productExists)

newtype Request = Request { getRequirements :: [Requirement] }
  deriving Show

data Requirement
  = Version (Source,Language)
  | Product (Source,Language,Product)
  deriving (Show,Eq,Ord)

allRequirementsForRequestAreFulfilled :: MessageStore -> Request -> Bool
allRequirementsForRequestAreFulfilled store = all (reqirementExist store) . getRequirements

reqirementExist :: MessageStore -> Requirement -> Bool
reqirementExist store requirement =
  case requirement of
    Version version -> versionExists version store
    Product product -> productExists product store
