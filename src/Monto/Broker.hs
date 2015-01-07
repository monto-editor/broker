{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Monto.Broker
  ( empty
  , register
  , Broker
  , newVersion
  , newProduct
  )
  where

import           Data.Graph.Inductive (Gr,Node)
import qualified Data.Graph.Inductive as G
import           Data.List (mapAccumR)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromJust,fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S

import           Monto.Types
import           Monto.Automaton (CompiledAutomaton,Process)
import qualified Monto.Automaton as A
import           Monto.VersionMessage (VersionMessage)
import qualified Monto.VersionMessage as V
import           Monto.ProductMessage (ProductMessage)
import qualified Monto.ProductMessage as P
import           Monto.ServerDependency
import           Monto.ResourceManager (ResourceManager)
import qualified Monto.ResourceManager as R

data Broker = Broker
  { servers      :: Map ServerDependency Node
  , dependencies :: Gr ServerDependency ()
  , maxNode      :: Node
  , resourceMgr  :: ResourceManager
  , automaton    :: CompiledAutomaton Server (Set Server)
  , processes    :: Map Source (Process Server (Set Server))
  } deriving (Eq,Show)

empty :: Broker
{-# INLINE empty #-}
empty = Broker
  { servers      = M.fromList [(Source,0),(Star,1)]
  , dependencies = G.insNodes [(0,Source),(1,Star)] G.empty
  , maxNode      = 1
  , resourceMgr  = R.empty
  , automaton    = A.empty
  , processes    = M.empty
  }

register :: Server -> [ServerDependency] -> Broker -> Broker
{-# INLINE register #-}
register server deps broker =
  let (broker',serverNode) = registerServer broker server
      (broker'',depNodes)  = mapAccumR registerServer broker' deps
      dependencies' = G.insNodes [(1,Star)]
                    $ G.insEdges [(serverNode,depNode,()) | depNode <- depNodes]
                    $ G.delNode 1
                    $ dependencies broker''
      dependencies'' = G.insEdges [(1,n,()) | n <- G.nodes dependencies', n /= 1 && G.indeg dependencies' n == 0 ] dependencies'
      auto = updateAutomaton dependencies''
  in broker''
    { dependencies = dependencies''
    , automaton    = auto
    , processes    = M.map (const (A.start auto)) (processes broker'')
    }

registerServer :: Broker -> Server -> (Broker,Node)
{-# INLINE registerServer #-}
registerServer broker server =
  case M.lookup server (servers broker) of
    Just node -> (broker,node)
    Nothing   ->
      let broker' = broker
            { servers      = M.insert server newNode (servers broker)
            , dependencies = G.insNode (newNode,server) (dependencies broker)
            , maxNode      = newNode
            }
      in (broker',newNode)
  where
    newNode = maxNode broker + 1

type Responses = [Server]

newVersion :: VersionMessage -> Broker -> (Responses,Broker)
{-# INLINE newVersion #-}
newVersion version broker =
  let process = fromMaybe (A.start (automaton broker))
              $ M.lookup (V.source version)
              $ processes broker
      (r,process') = fromMaybe (S.empty,process) $ A.runProcess Source process
      broker' = broker
        { resourceMgr = snd $ R.updateVersion version $ resourceMgr broker
        , processes   = M.insert (V.source version) process' (processes broker)
        }
  in (r,broker')

newProduct :: ProductMessage -> Broker -> (Responses,Broker)
{-# INLINE newProduct #-}
newProduct pr broker =
  let process = fromMaybe (A.start (automaton broker))
              $ M.lookup (P.source pr)
              $ processes broker
      (r,process') = fromMaybe (S.empty,process) $ A.runProcess (Server (P.product pr)) process
      broker' = broker
        { resourceMgr = snd $ R.updateProduct' pr $ resourceMgr broker
        , processes   = M.insert (P.product pr) process' (processes broker)
        }
  in (r,broker')

request :: 

updateAutomaton :: Gr ServerDependency () -> CompiledAutomaton Server (Set Server)
{-# INLINE updateAutomaton #-}
updateAutomaton gr =
  A.compileAutomaton $ foldr1 A.merge $ map (uncurry A.buildAutomaton . outEdges) $ G.bfs 0 $ G.grev gr
  where
    outEdges n =
      let (Just (_,_,a,out),_) = G.match n gr
      in (a,[ fromJust (G.lab gr o) | (_,o) <- out ])
