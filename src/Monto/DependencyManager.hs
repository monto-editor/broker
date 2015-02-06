{-# LANGUAGE FlexibleInstances #-}
module Monto.DependencyManager where

import           Control.Monad (guard)

import           Data.List (mapAccumR)
import           Data.Graph.Inductive (Gr,Node)
import qualified Data.Graph.Inductive as G
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import           Data.Maybe (fromJust)
import qualified Data.Text as T

import           Monto.Types
import           Monto.Automaton (Automaton(..),L)
import qualified Monto.Automaton as A

data Dependency dep
  = Dependency dep
  | Bottom
  | Top
  deriving (Eq,Ord,Show)

data ProductDependency
  = Version Source
  | Product (Source,Language,Product)
  deriving (Eq,Ord,Show)

data Server = Server Product Language
  deriving (Eq,Ord,Show)

instance Read dep => Read (Dependency dep) where
  readsPrec p r = do
    (a,r') <- lex r
    if a == "Source"
      then return (Bottom,r')
      else do
        (dep,r'')<- readsPrec p r
        return (Dependency dep,r'')

instance Read Server where
  readsPrec _ r = do
    (a,r') <- lex r
    (b,r'') <- lex r'
    (c,r''') <- lex r''
    guard $ b == "/"
    return (Server (T.pack a) (T.pack c),r''')

data Response = Response Server [ProductDependency]

data DependencyManager dep
  = DependencyManager
  { nodeMap      :: Map (Dependency dep) Node
  , maxNode      :: Node
  , dependencies :: Gr (Dependency dep) ()
  , automaton    :: Automaton (Map (Dependency dep) L) (Dependency dep) (Set (Dependency dep))
  } deriving (Eq,Show)

empty :: Ord dep => DependencyManager dep
empty = DependencyManager
  { nodeMap      = M.fromList [(Bottom,0),(Top,1)]
  , dependencies = G.insNodes [(0,Bottom),(1,Top)] G.empty
  , maxNode      = 1
  , automaton    = Automaton
      { initialState = M.empty
      , transitions  = []
      , accepting    = []
      }
  }

register :: Ord dep => dep -> [Dependency dep] -> DependencyManager dep -> DependencyManager dep
{-# INLINE register #-}
register from to manager =
  let (manager',fromNode) = registerDependency manager (Dependency from)
      (manager'',toNodes)  = mapAccumR registerDependency manager' to
      dependencies' = G.insNodes [(1,Top)]
                    $ G.insEdges [(fromNode,toNode,()) | toNode <- toNodes]
                    $ G.delNode 1
                    $ dependencies manager''
      dependencies'' = G.insEdges [(1,n,()) | n <- G.nodes dependencies', n /= 1 && G.indeg dependencies' n == 0 ] dependencies'
      auto = updateAutomaton dependencies''
  in manager''
    { dependencies = dependencies''
    , automaton    = auto
    }

registerDependency :: Ord dep => DependencyManager dep -> Dependency dep -> (DependencyManager dep,Node)
{-# INLINE registerDependency #-}
registerDependency manager dep =
  case M.lookup dep (nodeMap manager) of
    Just node -> (manager,node)
    Nothing   ->
      let manager' = manager
            { nodeMap      = M.insert dep newNode (nodeMap manager)
            , dependencies = G.insNode (newNode,dep) (dependencies manager)
            , maxNode      = newNode
            }
      in (manager',newNode)
  where
    newNode = maxNode manager + 1

updateAutomaton :: Ord dep => Gr dep () -> Automaton (Map dep L) dep (Set dep)
{-# INLINE updateAutomaton #-}
updateAutomaton gr =
  foldr1 A.merge $ map (uncurry A.buildAutomaton . outEdges) $ G.bfs 0 $ G.grev gr
  where
    outEdges n =
      let (Just (_,_,a,out),_) = G.match n gr
      in (a,[ fromJust (G.lab gr o) | (_,o) <- out ])

lookupDependencies :: Ord dep => Dependency dep -> DependencyManager dep -> [Dependency dep]
lookupDependencies dep manager =
  let depNode = nodeMap manager M.! dep
  in do
    successor <- G.suc (dependencies manager) depNode
    return $ fromJust $ G.lab (dependencies manager) successor
