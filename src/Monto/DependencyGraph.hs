module Monto.DependencyGraph where

import           Data.Graph.Inductive (Gr,Node)
import qualified Data.Graph.Inductive as G
import           Data.List (mapAccumR)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe

data DependencyGraph dep
  = DependencyGraph
  { maxNode :: Node
  , nodeMap      :: Map dep Node
  , dependencies :: Gr dep ()
  } deriving (Eq)

instance Show dep => Show (DependencyGraph dep) where
 show gr = G.prettify (dependencies gr)

empty :: Ord dep => DependencyGraph dep
empty = DependencyGraph
  { nodeMap      = M.empty
  , dependencies = G.empty
  , maxNode      = 1
  }

register :: Ord dep => dep -> [dep] -> DependencyGraph dep -> DependencyGraph dep
{-# INLINE register #-}
register from to gr =
  let (gr',fromNode) = registerDependency gr from
      (gr'',toNodes) = mapAccumR registerDependency gr' to
      dependencies' = G.insEdges [ (fromNode,toNode,()) | toNode <- toNodes]
                    $ G.delEdges [ (fromNode,suc) | suc <- G.suc (dependencies gr'') fromNode ]
                    $ dependencies gr''
  in gr''
    { dependencies = dependencies'
    }

filterDeps :: Ord dep => (dep -> Bool) -> DependencyGraph dep -> DependencyGraph dep
filterDeps predicate gr =
  let (delete,keep) = M.partitionWithKey (\k _ -> predicate k) (nodeMap gr)
  in gr { dependencies = G.delNodes (M.elems delete) (dependencies gr)
        , nodeMap = keep
        }

deregister :: Ord dep => dep -> DependencyGraph dep -> DependencyGraph dep
deregister dep gr = fromMaybe gr $ do
  node <- M.lookup dep (nodeMap gr)
  return $ gr { dependencies = G.delNode node (dependencies gr)
              , nodeMap = M.delete dep (nodeMap gr)
              }

registerDependency :: Ord dep => DependencyGraph dep -> dep -> (DependencyGraph dep,Node)
{-# INLINE registerDependency #-}
registerDependency gr dep =
  case M.lookup dep (nodeMap gr) of
    Just node -> (gr,node)
    Nothing   ->
      let gr' = gr
            { nodeMap      = M.insert dep newNode (nodeMap gr)
            , dependencies = G.insNode (newNode,dep) (dependencies gr)
            , maxNode      = newNode
            }
      in (gr',newNode)
  where
    newNode = maxNode gr + 1

lookupReverseDependencies :: Ord dep => dep -> DependencyGraph dep -> [dep]
lookupReverseDependencies = lookupGraph G.pre

lookupDependencies :: Ord dep => dep -> DependencyGraph dep -> [dep]
lookupDependencies = lookupGraph G.suc

lookupGraph :: Ord dep => (Gr dep () -> Node -> [Node]) -> dep -> DependencyGraph dep -> [dep]
lookupGraph direction dep gr = fromMaybe [] $ do
  depNode <- M.lookup dep (nodeMap gr)
  let rdeps = direction (dependencies gr) depNode
  mapM (G.lab (dependencies gr)) rdeps
