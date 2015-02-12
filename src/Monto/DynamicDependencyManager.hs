module Monto.DynamicDependencyManager where

import           Control.Applicative

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe,isJust)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)

import           Monto.Types (Source)
import           Monto.Automaton (Process,L)
import qualified Monto.Automaton as A
import           Monto.DependencyManager (DependencyManager,Dependency(..),ProductDependency(..),Response(..),Server(..))
import qualified Monto.DependencyManager as D
import           Monto.VersionMessage (VersionMessage)
import qualified Monto.VersionMessage as V
import           Monto.ProductMessage (ProductMessage)
import qualified Monto.ProductMessage as P
import           Monto.ResourceManager (ResourceManager)
import qualified Monto.ResourceManager as R

type Dep = Dependency ProductDependency

data DynamicDependencyManager
  = DynamicDependencyManager
  { dependencyMgr :: DependencyManager ProductDependency
  , process       :: Process (Map Dep L) Dep (Set Dep)
  } deriving (Eq,Show)

empty :: DynamicDependencyManager
empty = DynamicDependencyManager
  { dependencyMgr = D.empty
  , process = A.Process (M.fromList [(Bottom,A.Top),(Top,A.Bottom)]) (A.compileAutomaton (D.automaton D.empty))
  }

register :: ProductDependency
         -> [ProductDependency]
         -> ResourceManager
         -> DynamicDependencyManager
         -> ([Source],DynamicDependencyManager)
{-# INLINE register #-}
register from to resource manager =
  let mgr'     = D.connectLeafsToBottom $ D.register from (Dependency <$> to) (dependencyMgr manager)
      res      = flip map to $ \dep -> 
        case dep of
          Version src       -> src
          Product (src,_,_) -> src
      manager' = updateProcess resource $ manager { dependencyMgr = mgr' }
  in (res,manager')

newVersion :: VersionMessage
           -> DynamicDependencyManager
           -> ([Response],DynamicDependencyManager)
{-# INLINE newVersion #-}
newVersion version = newMessage (Version (V.source version))

newProduct :: ProductMessage
           -> DynamicDependencyManager
           -> ([Response],DynamicDependencyManager)
{-# INLINE newProduct #-}
newProduct pr = newMessage (Product (P.source pr, P.product pr, P.language pr))

newMessage :: ProductDependency -> DynamicDependencyManager -> ([Response],DynamicDependencyManager)
{-# INLINE newMessage #-}
newMessage dep manager = fromMaybe ([],manager) $ do
  (r,process') <- A.runProcess (Dependency dep) (process manager)
  let res = map (makeResponse manager) (S.toList r) 
      manager' = manager { process = process' }
  return (res,manager')

updateProcess :: ResourceManager -> DynamicDependencyManager -> DynamicDependencyManager
updateProcess resource manager = manager
  { process = A.Process newState (A.compileAutomaton auto) }
  where
    auto = D.automaton (dependencyMgr manager)
    current = A.currentState (process manager) 
    newState = M.mapWithKey dependencyAvailable
             $ M.unionWith A.leastUpperBound current (A.initialState auto)
    dependencyAvailable dep l = A.leastUpperBound l $ case dep of
      Dependency (Version src)  | isJust (R.lookupVersionMessage src resource)  -> A.Top
      Dependency (Product prod) | isJust (R.lookupProductMessage prod resource) -> A.Top
      Bottom                                                                    -> A.Top
      _                                                                         -> A.Bottom

makeResponse :: DynamicDependencyManager -> Dep -> Response
makeResponse manager dep@(Dependency (Product (src,lang,prod))) =
  let server = Server prod lang
      deps = D.lookupDependencies dep (dependencyMgr manager)
  in Response src server (map getProductDep deps)
makeResponse _ _ = error "cannot send response to bottom or top"

getProductDep :: Dependency ProductDependency -> ProductDependency
getProductDep (Dependency p) = p
getProductDep _ = error "Bottom or Top is no product dependency"

automatonToDot :: DynamicDependencyManager -> Text
automatonToDot = D.automatonToDot . dependencyMgr
