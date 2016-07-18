{-# LANGUAGE TupleSections #-}
module Monto.DependencyGraph where

import           Data.Graph.Inductive (Adj, Context, Gr, Node)
import qualified Data.Graph.Inductive as G
import           Data.List            (mapAccumR)
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe

data DependencyGraph v e
  = DependencyGraph
  { maxNode      :: Node
  , nodeMap      :: Map v Node
  , dependencies :: Gr v e
  } deriving (Eq)

instance (Show v, Show e) => Show (DependencyGraph v e) where
 show gr = G.prettify (dependencies gr)

empty :: Ord v => DependencyGraph v e
empty = DependencyGraph
  { nodeMap      = M.empty
  , dependencies = G.empty
  , maxNode      = 1
  }

register :: (Ord v) => v -> [(e,v)] -> DependencyGraph v e -> DependencyGraph v e
{-# INLINE register #-}
register from to gr =
  let (gr',fromNode) = registerNode gr from
      (gr'',toNodes) = mapAccumR registerNode gr' (map snd to)
      dependencies' = G.insEdges [ (fromNode,toNode,e) | (e,toNode) <- zip (map fst to) toNodes]
                    $ G.delEdges [ (fromNode,suc) | suc <- G.suc (dependencies gr'') fromNode ]
                    $ dependencies gr''
  in gr''
    { dependencies = dependencies'
    }

filterDeps :: Ord v => (v -> Bool) -> DependencyGraph v e -> DependencyGraph v e
filterDeps predicate gr =
  let (delete,keep) = M.partitionWithKey (\k _ -> predicate k) (nodeMap gr)
  in gr { dependencies = G.delNodes (M.elems delete) (dependencies gr)
        , nodeMap = keep
        }

deregister :: Ord v => v -> DependencyGraph v e -> DependencyGraph v e
deregister dep gr = fromMaybe gr $ do
  node <- M.lookup dep (nodeMap gr)
  return $ gr { dependencies = G.delNode node (dependencies gr)
              , nodeMap = M.delete dep (nodeMap gr)
              }

registerNode :: Ord v => DependencyGraph v e -> v -> (DependencyGraph v e,Node)
{-# INLINE registerNode #-}
registerNode gr v =
  case M.lookup v (nodeMap gr) of
    Just node -> (gr,node)
    Nothing   ->
      let gr' = gr
            { nodeMap      = M.insert v newNode (nodeMap gr)
            , dependencies = G.insNode (newNode,v) (dependencies gr)
            , maxNode      = newNode
            }
      in (gr',newNode)
  where
    newNode = maxNode gr + 1

lookupReverseDependencies :: (Ord v) => v -> DependencyGraph v e -> [(e,v)]
lookupReverseDependencies = lookupGraph (\(pre,_,_,_) -> pre)

lookupDependencies :: (Ord v) => v -> DependencyGraph v e -> [(e,v)]
lookupDependencies = lookupGraph (\(_,_,_,suc) -> suc)

lookupGraph :: (Ord v) => (Context v e -> Adj e) -> v -> DependencyGraph v e -> [(e,v)]
lookupGraph direction v gr = fromMaybe [] $ do
  node <- M.lookup v (nodeMap gr)
  ctx <- fst $ G.match node (dependencies gr)
  let adj = direction ctx
  mapM (\(e,n) -> (e,) <$> G.lab (dependencies gr) n) adj
