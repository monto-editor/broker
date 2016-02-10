{-# LANGUAGE CPP #-}
module Monto.ResourceManager
  ( ResourceManager
  , empty
  , isOutdated
  , updateVersion
  , updateProduct
  , updateProduct'
  , lookupVersionMessage
  , lookupProductMessage
  )
  where

import           Data.Map (Map)
import qualified Data.Map as M
#if __GLASGOW_HASKELL__ < 710
import           Data.Foldable (Foldable)
#endif
import qualified Data.Foldable as F
import           Data.Graph.Inductive (Gr,Node)
import qualified Data.Graph.Inductive as G
import           Data.Maybe(fromMaybe,mapMaybe)
import           Data.Vector (Vector)
import qualified Data.Vector as V

import           Monto.Types
import           Monto.ProductDependency
import           Monto.VersionMessage (VersionMessage)
import qualified Monto.VersionMessage as V
import           Monto.ProductMessage (ProductMessage)
import qualified Monto.ProductMessage as P

data ResourceManager = ResourceManager
  { versions     :: Map Source (VersionMessage,Node)
  , products     :: Map (Source,ServiceID,Product,Language) (ProductMessage,Node)
  , dependencies :: Gr ProductDependency ()
  , maxNode      :: Int
  }

empty :: ResourceManager
{-# INLINE empty #-}
empty = ResourceManager M.empty M.empty G.empty 0

isOutdated :: ProductMessage -> ResourceManager -> Bool
isOutdated pr resourceMgr = fromMaybe True $ do
  (v,_) <- M.lookup (P.source pr) (versions resourceMgr)
  return $ P.versionId pr < V.versionId v

updateVersion :: VersionMessage -> ResourceManager -> (Vector Invalid,ResourceManager)
{-# INLINE updateVersion #-}
updateVersion version resourceMgr =
  let dep = versionDependency version
      (invalid,resourceMgr') = removeOldDependencies dep resourceMgr
      resourceMgr''          = addVersion version resourceMgr'
  in (invalid,resourceMgr'')

updateProduct' :: ProductMessage -> ResourceManager -> (Vector Invalid,ResourceManager)
{-# INLINE updateProduct' #-}
updateProduct' p s = fromMaybe (V.empty,s) $ updateProduct p s

updateProduct :: ProductMessage -> ResourceManager -> Maybe (Vector Invalid,ResourceManager)
{-# INLINE updateProduct #-}
updateProduct prod resourceMgr = do
  let dep = productDependency prod
  let (invalid,resourceMgr') = removeOldDependencies dep resourceMgr
  resourceMgr'' <- addProduct prod resourceMgr'
  return (invalid,resourceMgr'')

addVersion :: VersionMessage -> ResourceManager -> ResourceManager
{-# INLINE addVersion #-}
addVersion version resourceMgr = resourceMgr
  { versions = M.insert (V.source version) (version,node) (versions resourceMgr)
  , dependencies = G.insNode (node,versionDependency version) (dependencies resourceMgr)
  , maxNode = node + 1
  }
  where
    node = maxNode resourceMgr

removeOldDependencies :: ProductDependency -> ResourceManager -> (Vector Invalid,ResourceManager)
{-# INLINE removeOldDependencies #-}
removeOldDependencies dep resourceMgr = fromMaybe (V.empty,resourceMgr) $ do
  let rdeps = reverseDependencies dep resourceMgr
  let invalid = V.fromList rdeps
  return (invalid,F.foldr removeProductDependency resourceMgr invalid)

removeProductDependency :: ReverseProductDependency -> ResourceManager -> ResourceManager
{-# INLINE removeProductDependency #-}
removeProductDependency (VersionDependency s _) resourceMgr = fromMaybe resourceMgr $ do
  node <- lookupVersionNode s resourceMgr
  return $ resourceMgr
    { versions     = M.delete s (versions resourceMgr)
    , dependencies = G.delNode node (dependencies resourceMgr)
    }
removeProductDependency (ProductDependency s sid prod lang) resourceMgr = fromMaybe resourceMgr $ do
  node <- lookupProductNode (s,sid,prod,lang) resourceMgr
  return $ resourceMgr
    { products     = M.delete (s,sid,prod,lang) (products resourceMgr)
    , dependencies = G.delNode node (dependencies resourceMgr)
    }

addProduct :: ProductMessage -> ResourceManager -> Maybe ResourceManager
{-# INLINE addProduct #-}
addProduct productMessage resourceMgr = do
  versionNode <- lookupVersionNode source resourceMgr
  version     <- G.lab (dependencies resourceMgr) versionNode
  addProductDependencyEdges node (V.cons version (P.dependencies productMessage)) $ resourceMgr
    { products     = M.insert (source,sid,prod,lang) (productMessage,node) (products resourceMgr)
    , dependencies = G.insNode (node,dep) (dependencies resourceMgr)
    , maxNode      = node + 1
    }
  where
    node   = maxNode resourceMgr
    sid    = P.serviceId productMessage
    source = P.source    productMessage
    prod   = P.product   productMessage
    lang   = P.language  productMessage
    dep    = productDependency productMessage

addProductDependencyEdges :: Foldable f => Node -> f ProductDependency -> ResourceManager -> Maybe ResourceManager
{-# INLINE addProductDependencyEdges #-}
addProductDependencyEdges from deps resourceMgr =
  F.foldlM addProductDependencyEdge resourceMgr deps
  where
    addProductDependencyEdge st (VersionDependency s _) = do
      to <- lookupVersionNode s resourceMgr
      return $ st { dependencies = G.insEdge (from,to,()) (dependencies st) }
    addProductDependencyEdge st (ProductDependency s sid prod lang) = do
      to <- lookupProductNode (s,sid,prod,lang) resourceMgr
      return $ st { dependencies = G.insEdge (from,to,()) (dependencies st) }

reverseDependencies :: ProductDependency -> ResourceManager -> [ReverseProductDependency]
{-# INLINE reverseDependencies #-}
reverseDependencies (VersionDependency s _) resourceMgr = fromMaybe [] $ do
  node <- lookupVersionNode s resourceMgr
  let deps = dependencies resourceMgr
  return $ mapMaybe (G.lab deps) $ G.rdfs [node] deps
reverseDependencies (ProductDependency s sid prod lang) resourceMgr = fromMaybe [] $ do
  node <- lookupProductNode (s,sid,prod,lang) resourceMgr
  let deps = dependencies resourceMgr
  return $ mapMaybe (G.lab deps) $ G.rdfs [node] deps

lookupVersionMessage :: Source -> ResourceManager -> Maybe VersionMessage
lookupVersionMessage s resourceMgr = do
  (version,_) <- M.lookup s (versions resourceMgr)
  return version

lookupProductMessage :: (Source,ServiceID,Product,Language) -> ResourceManager -> Maybe ProductMessage
lookupProductMessage s resourceMgr = do
  (prod,_) <- M.lookup s (products resourceMgr)
  return prod

-- Helper functions and instances

lookupVersionNode :: Source -> ResourceManager -> Maybe Node
lookupVersionNode s resourceMgr = do
  (_,node) <- M.lookup s (versions resourceMgr)
  return node

lookupProductNode :: (Source,ServiceID,Product,Language) -> ResourceManager -> Maybe Node
lookupProductNode k resourceMgr = do
  (_,node) <- M.lookup k (products resourceMgr)
  return node

versionDependency :: VersionMessage -> ProductDependency
versionDependency version = VersionDependency (V.source version) (V.language version)
{-# INLINE versionDependency #-}

productDependency :: ProductMessage -> ProductDependency
productDependency prod = ProductDependency (P.source prod) (P.serviceId prod) (P.product prod) (P.language prod)
{-# INLINE productDependency #-}

instance Eq ResourceManager where
  m1 == m2 = versions m1 == versions m2
          && products m1 == products m2
          && dependencies m1 == dependencies m2

instance Show ResourceManager where
  show (ResourceManager vs ps deps _) =
    unwords [ "ResourceManager", "\n"
            , show vs, "\n"
            , show ps, "\n"
            , show deps
            ]
