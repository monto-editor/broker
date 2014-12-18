module Monto.MessageStore where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Foldable (Foldable)
import qualified Data.Foldable as F
import           Data.Graph.Inductive (Gr,Node)
import qualified Data.Graph.Inductive as G
import           Data.Maybe(catMaybes,fromMaybe)
import           Data.Vector (Vector)
import qualified Data.Vector as V

import           Monto.Dependency
import           Monto.VersionMessage (VersionMessage)
import qualified Monto.VersionMessage as V
import           Monto.ProductMessage (ProductMessage)
import qualified Monto.ProductMessage as P

-- | An edge from a product a to b means, that a depends on b
type DependencyGraph = Gr Dependency ()

data MessageStore = MessageStore
  { versions     :: Map Source                    Node
  , products     :: Map (Source,Language,Product) Node
  , dependencies :: DependencyGraph
  , maxNode      :: Int
  }

empty :: MessageStore
{-# INLINE empty #-}
empty = MessageStore M.empty M.empty G.empty 0

updateVersion :: VersionMessage -> MessageStore -> (Vector Invalid,MessageStore)
{-# INLINE updateVersion #-}
updateVersion version store =
  let vdep             = Version $ versionId version
      (invalid,store') = removeOldDependencies vdep store
      store''          = addVersion version store'
  in (invalid,store'')

updateProduct' :: ProductMessage -> MessageStore -> (Vector Invalid,MessageStore)
{-# INLINE updateProduct' #-}
updateProduct' p s = fromMaybe (V.empty,s) $ updateProduct p s

updateProduct :: ProductMessage -> MessageStore -> Maybe (Vector Invalid,MessageStore)
{-# INLINE updateProduct #-}
updateProduct prod store = do
  let pid              = productId prod
  let (invalid,store') = removeOldDependencies (Product pid) store
  store'' <- addProduct prod store'
  return (invalid,store'')

addVersion :: VersionMessage -> MessageStore -> MessageStore
{-# INLINE addVersion #-}
addVersion version store = store
  { versions = M.insert source node (versions store)
  , dependencies = G.insNode (node,Version (vid,source,lang)) (dependencies store)
  , maxNode = node + 1
  }
  where
    node   = maxNode store
    vid    = V.versionId version
    lang   = V.language version
    source = V.source version

removeOldDependencies :: Dependency -> MessageStore -> (Vector Invalid,MessageStore)
{-# INLINE removeOldDependencies #-}
removeOldDependencies dep store = fromMaybe (V.empty,store) $ do
  let rdeps = reverseDependencies dep store
  old <- case dep of
    Version (_,s,_) -> do
      node <- M.lookup s (versions store)
      G.lab (dependencies store) node
    Product (_,_,s,l,p) -> do
      node <- M.lookup (s,l,p) (products store)
      G.lab (dependencies store) node
  let invalid = V.fromList $ old:rdeps
  return (invalid,F.foldr removeDependency store invalid)

removeDependency :: ReverseDependency -> MessageStore -> MessageStore
{-# INLINE removeDependency #-}
removeDependency (Version (_,s,_)) store = fromMaybe store $ do
  node <- M.lookup s (versions store)
  return $ store
    { versions     = M.delete s (versions store)
    , dependencies = G.delNode node (dependencies store)
    }
removeDependency (Product (_,_,s,l,p)) store = fromMaybe store $ do
  node <- M.lookup (s,l,p) (products store)
  return $ store
    { products     = M.delete (s,l,p) (products store)
    , dependencies = G.delNode node (dependencies store)
    }

addProduct :: ProductMessage -> MessageStore -> Maybe MessageStore
{-# INLINE addProduct #-}
addProduct productMessage store = do
  versionNode <- M.lookup source (versions store)
  version     <- G.lab (dependencies store) versionNode
  addDependencyEdges node (V.cons version (P.productDependencies productMessage)) $ store
    { products     = M.insert (source,lang,prod) node (products store)
    , dependencies = G.insNode (node,dep) (dependencies store)
    , maxNode      = node + 1
    }
  where
    node   = maxNode store
    vid    = P.versionId productMessage
    pid    = P.productId productMessage
    lang   = P.language  productMessage
    source = P.source    productMessage
    prod   = P.product   productMessage
    dep    = Product (vid,pid,source,lang,prod)

addDependencyEdges :: Foldable f => Node -> f Dependency -> MessageStore -> Maybe MessageStore
{-# INLINE addDependencyEdges #-}
addDependencyEdges from deps store =
  F.foldlM addDependencyEdge store deps
  where
    addDependencyEdge st (Version (_,s,_)) = do
      to <- M.lookup s (versions store)
      return $ st { dependencies = G.insEdge (from,to,()) (dependencies st) }
    addDependencyEdge st (Product (_,_,s,l,p)) = do
      to <- M.lookup (s,l,p) (products store)
      return $ st { dependencies = G.insEdge (from,to,()) (dependencies st) }

reverseDependencies :: Dependency -> MessageStore -> [ReverseDependency]
{-# INLINE reverseDependencies #-}
reverseDependencies (Version (_,s,_)) store = fromMaybe [] $ do
  node <- M.lookup s (versions store)
  let deps = dependencies store
  return $ catMaybes $ map (G.lab deps) $ G.rdfs [node] deps
reverseDependencies (Product (_,_,s,l,p)) store = fromMaybe [] $ do
  node <- M.lookup (s,l,p) (products store)
  let deps = dependencies store
  return $ catMaybes $ map (G.lab deps) $ G.rdfs [node] deps

-- Helper functions and instances

versionId :: VersionMessage -> (VersionID,Source,Language)
versionId version = (V.versionId version,V.source version,V.language version)
{-# INLINE versionId #-}

productId :: ProductMessage -> (VersionID,ProductID,Source,Product,Language)
productId prod = (P.productId prod, P.versionId prod, P.source prod,P.product prod, P.language prod)
{-# INLINE productId #-}

instance Eq MessageStore where
  m1 == m2 = versions m1 == versions m2
          && products m1 == products m2
          && dependencies m1 == dependencies m2

instance Show MessageStore where
  show (MessageStore vs ps deps _) =
    unwords [ "MessageStore", "\n"
            , show vs, "\n"
            , show ps, "\n"
            , show deps
            ]
