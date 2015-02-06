module Monto.DynamicDependencyManager where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
--import qualified Data.Set as S

import           Monto.Types
import           Monto.Automaton (Automaton(..),Process,L)
import qualified Monto.Automaton as A
import           Monto.DependencyManager (DependencyManager,Dependency)
import qualified Monto.DependencyManager as D

data ProductDependency
  = Version (Source,Language)
  | Product (Source,Language,Product)
  deriving (Eq,Ord,Show)

type Dep = Dependency ProductDependency

data DynamicDependencyManager
  = DynamicDependencyManager
  { dependencyMgr :: DependencyManager ProductDependency
  , process       :: Process (Map Dep L) Dep (Set Dep)
  }

empty :: DynamicDependencyManager
empty = DynamicDependencyManager
  { dependencyMgr = D.empty
  , process = undefined
  }

register :: ProductDependency -> [ProductDependency] -> DynamicDependencyManager -> DynamicDependencyManager
{-# INLINE register #-}
register from to manager =
  let mgr' = D.register from to (dependencyMgr manager)
      auto = D.automaton mgr'
  in manager
  { dependencyMgr = mgr'
  , process       = A.Process (migrateState (A.currentState (process manager)) auto) (A.compileAutomaton auto)
  }

migrateState :: Ord dep
             => Map dep L
             -> Automaton (Map dep L) dep (Set dep)
             -> Map dep L
migrateState old newAuto = M.unionWith A.leastUpperBound old (A.initialState newAuto)

newVersion :: VersionMessage -> StaticDependencyManager -> ([Response],StaticDependencyManager)
{-# INLINE newVersion #-}
newVersion version manager = fromMaybe manager $ do
  (r,process') <- A.runProcess vid process
  (map (makeResponse manager') (S.toList r), manager { process = process' })
  where
    vid = (V.source version,V.language version)
