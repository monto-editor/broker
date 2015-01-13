module Monto.ResourceManager
  ( ResourceManager
  , empty
  , updateVersion
  , updateProduct
  , updateProduct'
  , lookupVersionMessage
  , lookupProductMessage
  )
  where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Foldable (Foldable)
import qualified Data.Foldable as F
import           Data.Graph.Inductive (Gr,Node)
import qualified Data.Graph.Inductive as G
import           Data.Maybe(catMaybes,fromMaybe)
import           Data.Vector (Vector)
import qualified Data.Vector as V

import           Monto.Types
import           Monto.ProductDependency
import           Monto.VersionMessage (VersionMessage)
import qualified Monto.VersionMessage as V
import           Monto.ProductMessage (ProductMessage)
import qualified Monto.ProductMessage as P

data ResourceManager = ResourceManager
  { versions     :: Map Source                    (VersionMessage,Node)
  , products     :: Map (Source,Language,Product) (ProductMessage,Node)
  , dependencies :: Gr ProductDependency ()
  , maxNode      :: Int
  }

empty :: ResourceManager
{-# INLINE empty #-}
empty = ResourceManager M.empty M.empty G.empty 0

updateVersion :: VersionMessage -> ResourceManager -> (Vector Invalid,ResourceManager)
{-# INLINE updateVersion #-}
updateVersion version resourceMgr =
  let vdep             = Version $ versionId version
      (invalid,resourceMgr') = removeOldDependencies vdep resourceMgr
      resourceMgr''          = addVersion version resourceMgr'
  in (invalid,resourceMgr'')

updateProduct' :: ProductMessage -> ResourceManager -> (Vector Invalid,ResourceManager)
{-# INLINE updateProduct' #-}
updateProduct' p s = fromMaybe (V.empty,s) $ updateProduct p s

updateProduct :: ProductMessage -> ResourceManager -> Maybe (Vector Invalid,ResourceManager)
{-# INLINE updateProduct #-}
updateProduct prod resourceMgr = do
  let pid              = productId prod
  let (invalid,resourceMgr') = removeOldDependencies (Product pid) resourceMgr
  resourceMgr'' <- addProduct prod resourceMgr'
  return (invalid,resourceMgr'')

addVersion :: VersionMessage -> ResourceManager -> ResourceManager
{-# INLINE addVersion #-}
addVersion version resourceMgr = resourceMgr
  { versions = M.insert source (version,node) (versions resourceMgr)
  , dependencies = G.insNode (node,Version (vid,source,lang)) (dependencies resourceMgr)
  , maxNode = node + 1
  }
  where
    node   = maxNode resourceMgr
    vid    = V.versionId version
    lang   = V.language version
    source = V.source version

removeOldDependencies :: ProductDependency -> ResourceManager -> (Vector Invalid,ResourceManager)
{-# INLINE removeOldDependencies #-}
removeOldDependencies dep resourceMgr = fromMaybe (V.empty,resourceMgr) $ do
  let rdeps = reverseDependencies dep resourceMgr
  {-old <- case dep of-}
    {-Version (_,s,_) -> do-}
      {-node <- M.lookup s (versions resourceMgr)-}
      {-G.lab (dependencies resourceMgr) node-}
    {-Product (_,_,s,l,p) -> do-}
      {-node <- M.lookup (s,l,p) (products resourceMgr)-}
      {-G.lab (dependencies resourceMgr) node-}
  let invalid = V.fromList $ rdeps
  return (invalid,F.foldr removeProductDependency resourceMgr invalid)

removeProductDependency :: ReverseProductDependency -> ResourceManager -> ResourceManager
{-# INLINE removeProductDependency #-}
removeProductDependency (Version (_,s,_)) resourceMgr = fromMaybe resourceMgr $ do
  node <- lookupVersionNode s resourceMgr
  return $ resourceMgr
    { versions     = M.delete s (versions resourceMgr)
    , dependencies = G.delNode node (dependencies resourceMgr)
    }
removeProductDependency (Product (_,_,s,l,p)) resourceMgr = fromMaybe resourceMgr $ do
  node <- lookupProductNode (s,l,p) resourceMgr
  return $ resourceMgr
    { products     = M.delete (s,l,p) (products resourceMgr)
    , dependencies = G.delNode node (dependencies resourceMgr)
    }

addProduct :: ProductMessage -> ResourceManager -> Maybe ResourceManager
{-# INLINE addProduct #-}
addProduct productMessage resourceMgr = do
  versionNode <- lookupVersionNode source resourceMgr
  version     <- G.lab (dependencies resourceMgr) versionNode
  addProductDependencyEdges node (V.cons version (P.productDependencies productMessage)) $ resourceMgr
    { products     = M.insert (source,lang,prod) (productMessage,node) (products resourceMgr)
    , dependencies = G.insNode (node,dep) (dependencies resourceMgr)
    , maxNode      = node + 1
    }
  where
    node   = maxNode resourceMgr
    vid    = P.versionId productMessage
    pid    = P.productId productMessage
    lang   = P.language  productMessage
    source = P.source    productMessage
    prod   = P.product   productMessage
    dep    = Product (vid,pid,source,lang,prod)

addProductDependencyEdges :: Foldable f => Node -> f ProductDependency -> ResourceManager -> Maybe ResourceManager
{-# INLINE addProductDependencyEdges #-}
addProductDependencyEdges from deps resourceMgr =
  F.foldlM addProductDependencyEdge resourceMgr deps
  where
    addProductDependencyEdge st (Version (_,s,_)) = do
      to <- lookupVersionNode s resourceMgr
      return $ st { dependencies = G.insEdge (from,to,()) (dependencies st) }
    addProductDependencyEdge st (Product (_,_,s,l,p)) = do
      to <- lookupProductNode (s,l,p) resourceMgr
      return $ st { dependencies = G.insEdge (from,to,()) (dependencies st) }

reverseDependencies :: ProductDependency -> ResourceManager -> [ReverseProductDependency]
{-# INLINE reverseDependencies #-}
reverseDependencies (Version (_,s,_)) resourceMgr = fromMaybe [] $ do
  node <- lookupVersionNode s resourceMgr
  let deps = dependencies resourceMgr
  return $ catMaybes $ map (G.lab deps) $ G.rdfs [node] deps
reverseDependencies (Product (_,_,s,l,p)) resourceMgr = fromMaybe [] $ do
  node <- lookupProductNode (s,l,p) resourceMgr
  let deps = dependencies resourceMgr
  return $ catMaybes $ map (G.lab deps) $ G.rdfs [node] deps

lookupVersionMessage :: Source -> ResourceManager -> Maybe VersionMessage
lookupVersionMessage s resourceMgr = do
  (version,_) <- M.lookup s (versions resourceMgr)
  return version

lookupProductMessage :: (Source,Language,Product) -> ResourceManager -> Maybe ProductMessage
lookupProductMessage s resourceMgr = do
  (prod,_) <- M.lookup s (products resourceMgr)
  return prod

-- Helper functions and instances

lookupVersionNode :: Source -> ResourceManager -> Maybe Node
lookupVersionNode s resourceMgr = do
  (_,node) <- M.lookup s (versions resourceMgr)
  return node

lookupProductNode :: (Source,Language,Product) -> ResourceManager -> Maybe Node
lookupProductNode k resourceMgr = do
  (_,node) <- M.lookup k (products resourceMgr)
  return node

versionId :: VersionMessage -> (VersionID,Source,Language)
versionId version = (V.versionId version,V.source version,V.language version)
{-# INLINE versionId #-}

productId :: ProductMessage -> (VersionID,ProductID,Source,Product,Language)
productId prod = (P.productId prod, P.versionId prod, P.source prod,P.product prod, P.language prod)
{-# INLINE productId #-}

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
