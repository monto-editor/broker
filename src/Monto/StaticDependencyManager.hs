{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Monto.StaticDependencyManager
  ( empty
  , StaticDependencyManager
  , register
  , newVersion
  , newProduct
  , Response(..)
  ) where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromJust,fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S

import           Monto.Types
import           Monto.Automaton (Process,L)
import qualified Monto.Automaton as A
import           Monto.VersionMessage (VersionMessage)
import qualified Monto.VersionMessage as V
import           Monto.ProductMessage (ProductMessage)
import qualified Monto.ProductMessage as P
import           Monto.DependencyManager (DependencyManager,Dependency)
import qualified Monto.DependencyManager as D

data Server = Server { product :: Product, language :: Language }
  deriving (Eq,Ord,Show)

type Dep = Dependency Server

data StaticDependencyManager
  = StaticDependencyManager
  { dependencyMgr :: DependencyManager Server
  , processes     :: Map Source (Process (Map Dep L) Dep (Set Dep))
  } deriving (Eq,Show)

data Response = Response Dep [Dep]

empty :: StaticDependencyManager
{-# INLINE empty #-}
empty = StaticDependencyManager
  { dependencyMgr = D.empty
  , processes     = M.empty
  }

register :: Server -> [Server] -> StaticDependencyManager -> StaticDependencyManager
{-# INLINE register #-}
register from to manager =
  let mgr' = D.register from to (dependencyMgr manager)
      auto = A.compileAutomaton (D.automaton mgr')
  in manager
    { dependencyMgr = mgr'
    , processes     = M.map (const (A.start auto)) (processes manager)
    }

newVersion :: VersionMessage -> StaticDependencyManager -> ([Response],StaticDependencyManager)
{-# INLINE newVersion #-}
newVersion version manager =
  let process = A.start (A.compileAutomaton (D.automaton (dependencyMgr manager)))
      (r,process') = fromMaybe (S.empty,process) $ A.runProcess D.Bottom process
      manager' = manager { processes = M.insert (V.source version) process' (processes manager) }
  in (map (makeResponse manager') (S.toList r),manager')

newProduct :: ProductMessage -> StaticDependencyManager -> ([Response],StaticDependencyManager)
{-# INLINE newProduct #-}
newProduct pr manager =
  let process = fromMaybe (A.start (A.compileAutomaton (D.automaton (dependencyMgr manager))))
              $ M.lookup (P.source pr)
              $ processes manager
      (r,process') = fromMaybe (S.empty,process) $ A.runProcess (D.Dependency (Server (P.product pr) (P.language pr))) process
      manager' = manager { processes   = M.insert (P.product pr) process' (processes manager) }
  in (map (makeResponse manager') (S.toList r),manager')

makeResponse :: StaticDependencyManager -> Dep -> Response
makeResponse manager server =
  Response server (fromJust (D.lookupDependencies server (dependencyMgr manager)))
