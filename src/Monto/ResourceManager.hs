{-# LANGUAGE CPP #-}
module Monto.ResourceManager
  ( ResourceManager
  , empty
  , isOutdated
  , updateVersion
  , updateProduct
  , lookupVersionMessage
  , lookupProductMessage
  )
  where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe

import           Monto.Types
import           Monto.VersionMessage (VersionMessage)
import qualified Monto.VersionMessage as V
import           Monto.ProductMessage (ProductMessage)
import qualified Monto.ProductMessage as P

data ResourceManager = ResourceManager
  { versions     :: Map Source VersionMessage
  , products     :: Map (Source,ServiceID,Product,Language) ProductMessage
  } deriving (Show,Eq)

empty :: ResourceManager
{-# INLINE empty #-}
empty = ResourceManager M.empty M.empty

isOutdated :: ProductMessage -> ResourceManager -> Bool
isOutdated pr resourceMgr = fromMaybe True $ do
  v <- M.lookup (P.source pr) (versions resourceMgr)
  return $ P.versionId pr < V.versionId v

updateVersion :: VersionMessage -> ResourceManager -> ResourceManager
{-# INLINE updateVersion #-}
updateVersion version resourceMgr@ResourceManager {versions = vs} =
  resourceMgr { versions = M.insert (V.source version) version vs }

updateProduct :: ProductMessage -> ResourceManager -> ResourceManager
{-# INLINE updateProduct #-}
updateProduct prod resourceMgr@ResourceManager {products = ps} =
  resourceMgr { products = M.insert k prod ps }
  where
    k = (P.source prod,P.serviceId prod,P.product prod,P.language prod)

lookupVersionMessage :: Source -> ResourceManager -> Maybe VersionMessage
lookupVersionMessage src resourceMgr = M.lookup src (versions resourceMgr)

lookupProductMessage :: (Source,ServiceID,Product,Language) -> ResourceManager -> Maybe ProductMessage
lookupProductMessage k resourceMgr = M.lookup k (products resourceMgr)
