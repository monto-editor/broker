module Monto.DynamicDependencyManager where

import           Control.Applicative

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S

import           Monto.Automaton (Automaton(..),Process,L)
import qualified Monto.Automaton as A
import           Monto.DependencyManager (DependencyManager,Dependency(..),ProductDependency(..),Response(..),Server(..))
import qualified Monto.DependencyManager as D
import           Monto.VersionMessage (VersionMessage)
import qualified Monto.VersionMessage as V
import           Monto.ProductMessage (ProductMessage)
import qualified Monto.ProductMessage as P

type Dep = Dependency ProductDependency

data DynamicDependencyManager
  = DynamicDependencyManager
  { dependencyMgr :: DependencyManager ProductDependency
  , process       :: Process (Map Dep L) Dep (Set Dep)
  } deriving (Eq,Show)

empty :: DynamicDependencyManager
empty = DynamicDependencyManager
  { dependencyMgr = D.empty
  , process = A.Process M.empty (A.compileAutomaton (D.automaton D.empty))
  }

register :: ProductDependency
         -> [ProductDependency]
         -> DynamicDependencyManager
         -> DynamicDependencyManager
{-# INLINE register #-}
register from to manager =
  let mgr' = D.register from (Dependency <$> to) (dependencyMgr manager)
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

newVersion :: VersionMessage
           -> DynamicDependencyManager
           -> ([Response],DynamicDependencyManager)
{-# INLINE newVersion #-}
newVersion version manager = fromMaybe ([],manager) $ do
  (r,process') <- A.runProcess (Dependency vid) (process manager)
  let manager' = manager { process = process' }
      res = map (makeResponse manager') (S.toList r)
  return (res, manager')
  where
    vid = Version (V.source version)


newProduct :: ProductMessage
           -> DynamicDependencyManager
           -> ([Response],DynamicDependencyManager)
{-# INLINE newProduct #-}
newProduct pr manager = fromMaybe ([],manager) $ do
  (r,process') <- A.runProcess (Dependency pid) (process manager)
  let manager' = manager { process = process' }
      res = map (makeResponse manager') (S.toList r) 
  return (res,manager')
  where
    pid = Product (P.source pr, P.language pr, P.product pr)


makeResponse :: DynamicDependencyManager -> Dep -> Response
makeResponse manager dep@(Dependency (Product (_,lang,prod))) =
  let server = Server prod lang
      deps = D.lookupDependencies dep (dependencyMgr manager)
  in Response server (map getProductDep deps)
makeResponse _ _ = error "cannot send response to bottom or top"

getProductDep :: Dependency ProductDependency -> ProductDependency
getProductDep (Dependency p) = p
getProductDep _ = error "Bottom or Top is no product dependency"
