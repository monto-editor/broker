{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Monto.StaticDependencyManager
  ( empty
  , StaticDependencyManager
  , Server(..)
  , register
  , newVersion
  , newProduct
  , automatonToDot
  , lookupDependencies
  ) where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)

import           Monto.Types
import           Monto.Automaton (Process,L)
import qualified Monto.Automaton as A
import           Monto.VersionMessage (VersionMessage)
import qualified Monto.VersionMessage as V
import           Monto.ProductMessage (ProductMessage)
import qualified Monto.ProductMessage as P
import           Monto.DependencyManager (DependencyManager,Dependency(..),ProductDependency(..),Response(..),Server(..))
import qualified Monto.DependencyManager as D

type Dep = Dependency Server

data StaticDependencyManager
  = StaticDependencyManager
  { dependencyMgr :: DependencyManager Server
  , processes     :: Map Source (Process (Map Dep L) Dep (Set Dep))
  } deriving (Eq,Show)

empty :: StaticDependencyManager
{-# INLINE empty #-}
empty = StaticDependencyManager
  { dependencyMgr = D.empty
  , processes     = M.empty
  }

register :: Server -> [Dependency Server] -> StaticDependencyManager -> StaticDependencyManager
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
      (r,process') = fromMaybe (S.empty,process) $ A.runProcess Bottom process
      manager' = manager { processes = M.insert (V.source version) process' (processes manager) }
  in (map (makeResponse manager' (V.source version)) (S.toList r),manager')

newProduct :: ProductMessage -> StaticDependencyManager -> ([Response],StaticDependencyManager)
{-# INLINE newProduct #-}
newProduct pr manager =
  let process = fromMaybe (A.start (A.compileAutomaton (D.automaton (dependencyMgr manager))))
              $ M.lookup (P.source pr)
              $ processes manager
      (r,process') = fromMaybe (S.empty,process) $ A.runProcess (Dependency (Server (P.product pr) (P.language pr))) process
      manager' = manager { processes   = M.insert (P.product pr) process' (processes manager) }
  in (map (makeResponse manager' (P.source pr)) (S.toList r),manager')

makeResponse :: StaticDependencyManager -> Source -> Dep -> Response
makeResponse manager source s@(Dependency server) =
  let deps = D.lookupDependencies s (dependencyMgr manager)
  in Response source server (map (depForServer source) deps)
makeResponse _ _ _ = error "Cannot make request"

lookupDependencies :: Source -> Server -> StaticDependencyManager -> [ProductDependency]
lookupDependencies src srv = map (depForServer src) . D.lookupDependencies (Dependency srv) . dependencyMgr

depForServer :: Source -> Dep -> ProductDependency
depForServer source (Dependency (Server prod lang)) = Product (source,prod,lang)
depForServer source Bottom                          = Version source
depForServer _      Top                             = error "top is no dependency"

automatonToDot :: StaticDependencyManager -> Text
automatonToDot = D.automatonToDot . dependencyMgr
