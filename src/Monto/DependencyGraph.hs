module Monto.DependencyGraph where

import           Data.Graph.Inductive (Gr,Node)
import qualified Data.Graph.Inductive as G
import           Data.List (mapAccumR)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe

data Dependency dep
  = Dependency dep
  | Bottom
  | Top
  deriving (Eq,Ord)

instance Show dep => Show (Dependency dep) where
  show Bottom = "Bot"
  show Top = "Top"
  show (Dependency dep) = show dep

instance Read dep => Read (Dependency dep) where
  readsPrec p r = do
    (a,r') <- lex r
    if a == "Source"
      then return (Bottom,r')
      else do
        (dep,r'')<- readsPrec p r
        return (Dependency dep,r'')

data DependencyGraph dep
  = DependencyGraph
  { maxNode :: Node
  , nodeMap      :: Map (Dependency dep) Node
  , dependencies :: Gr (Dependency dep) ()
  } deriving (Show,Eq)

empty :: Ord dep => DependencyGraph dep
empty = DependencyGraph
  { nodeMap      = M.fromList [(Bottom,0),(Top,1)]
  , dependencies = G.insNodes [(0,Bottom),(1,Top)] G.empty
  , maxNode      = 1
  }

register :: Ord dep => dep -> [Dependency dep] -> DependencyGraph dep -> DependencyGraph dep
{-# INLINE register #-}
register from to gr =
  let (gr',fromNode) = registerDependency gr (Dependency from)
      (gr'',toNodes) = mapAccumR registerDependency gr' to
      dependencies' = G.insNodes [ (1,Top) ]
                    $ G.insEdges [ (fromNode,toNode,()) | toNode <- toNodes]
                    $ G.delNode 1
                    $ G.delEdges [ (fromNode,suc) | suc <- G.suc (dependencies gr'') fromNode ]
                    $ dependencies gr''
      dependencies'' = G.insEdges [(1,n,()) | n <- G.nodes dependencies'
                                            , n /= 1
                                            , n /= 0
                                            , G.indeg dependencies' n == 0 ]
                                  dependencies'
  in gr''
    { dependencies = dependencies''
    }

deregister :: Ord dep => dep -> DependencyGraph dep -> DependencyGraph dep
deregister dep gr = fromMaybe gr $ do
  node <- M.lookup (Dependency dep) (nodeMap gr)
  return $ gr { dependencies = G.delNode node (dependencies gr)
              , nodeMap = M.delete (Dependency dep) (nodeMap gr)
              }

registerDependency :: Ord dep => DependencyGraph dep -> Dependency dep -> (DependencyGraph dep,Node)
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
