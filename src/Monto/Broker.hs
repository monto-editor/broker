
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Monto.Broker
  ( empty
  , printBroker
  , registerService
  , deregisterService
  , registerDynamicDependency
  , printDependencyGraph
  , Broker(..)
  , newVersion
  , newProduct
  , Service(..)
  )
  where

import           Prelude hiding (product)

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative hiding (empty)
#endif

import           Data.Maybe
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as M

import           Monto.Types
import           Monto.SourceMessage (SourceMessage)
import qualified Monto.SourceMessage as SM
import           Monto.ProductMessage (ProductMessage)
import qualified Monto.ProductMessage as P
import           Monto.ResourceManager (ResourceManager)
import qualified Monto.ProductDependency as PD
import qualified Monto.ResourceManager as R
import           Monto.DependencyGraph (DependencyGraph)
import qualified Monto.DependencyGraph as DG
import           Monto.RegisterServiceRequest (RegisterServiceRequest)
import qualified Monto.RegisterServiceRequest as RQ
import           Monto.Service(Service(Service))
import qualified Monto.Service as S
import           Monto.Request (Request)
import qualified Monto.Request as Req

type Edge = (Product, Language)
type DynamicNode = (Source, ServiceID)
type ProductNode = ServiceID

data Broker = Broker
  { resourceMgr         :: ResourceManager
  , productDependencies :: DependencyGraph ProductNode [Edge]
  , dynamicDependencies :: DependencyGraph DynamicNode [Edge]
  , services            :: Map ServiceID Service
  , portPool            :: [Port]
  , serviceOnPort       :: Map Port ServiceID
  } deriving (Eq,Show)

empty :: Port -> Port -> Broker
{-# INLINE empty #-}
empty from to = Broker
  { resourceMgr = R.empty
  , productDependencies = DG.register "source" [] DG.empty
  , dynamicDependencies = DG.empty
  , services = M.empty
  , portPool = [from..to]
  , serviceOnPort = M.empty
  }

printBroker :: Broker -> IO()
printBroker broker = do
  print (services broker)
  print (portPool broker)
  print (productDependencies broker)

registerRequestToService :: Port -> RegisterServiceRequest -> Service
registerRequestToService port r =
  Service (RQ.serviceID r) (RQ.label r) (RQ.description r) (RQ.products r) port (RQ.options r)

dependenciesOfService :: RegisterServiceRequest -> [([Edge], ProductNode)]
dependenciesOfService = fmap productDependencyToTuple . RQ.dependencies
  where
    productDependencyToTuple (PD.ProductDependency sid prod lang) = ([(prod,lang)],sid)

registerService :: RegisterServiceRequest -> Broker -> Broker
{-# INLINE registerService #-}
registerService register broker =
  case portPool broker of
    (port:restPool) ->
        let serviceID = RQ.serviceID register
            service = registerRequestToService port register
            deps = dependenciesOfService register
        in broker
               { productDependencies = DG.register serviceID deps (productDependencies broker)
               , services = M.insert serviceID service (services broker)
               , portPool = restPool
               , serviceOnPort = M.insert port serviceID (serviceOnPort broker)
               }
    [] -> error "no more ports avaialable"

deregisterService :: ServiceID -> Broker -> Broker
{-# INLINE deregisterService #-}
deregisterService serviceID broker = fromMaybe broker $ do
  service <- M.lookup serviceID (services broker)
  return broker
    { services = M.delete serviceID (services broker)
    , portPool = List.insert (S.port service) (portPool broker)
    , productDependencies = DG.deregister serviceID (productDependencies broker)
    , serviceOnPort = M.delete (S.port service) (serviceOnPort broker)
    }

newVersion :: SourceMessage -> Broker -> ([Request],Broker)
{-# INLINE newVersion #-}
newVersion srcMsg broker =
  let broker' = broker
        { resourceMgr = R.updateSource srcMsg $ resourceMgr broker
        }
      source = SM.source srcMsg
      serviceID = "source"
      language = SM.language srcMsg
  in (servicesWithSatisfiedDependencies source serviceID language broker', broker')

newProduct :: ProductMessage -> Broker -> ([Request],Broker)
{-# INLINE newProduct #-}
newProduct pr broker
  | R.isOutdated pr (resourceMgr broker) = ([],broker)
  | otherwise =
    let broker' = broker
          { resourceMgr = R.updateProduct pr $ resourceMgr broker
          }
        source = P.source pr
        serviceID = P.serviceID pr
        language = P.language pr
    in (servicesWithSatisfiedDependencies source serviceID language broker', broker')

servicesWithSatisfiedDependencies :: Source -> ServiceID -> Language -> Broker -> [Request]
servicesWithSatisfiedDependencies source serviceID language broker =
  let reverseDeps = reverseDependenciesOf source serviceID language broker :: [([Edge], DynamicNode)]
      depsOfReverseDeps = dependenciesOf reverseDeps language broker :: [(([Edge], DynamicNode), [([Edge], DynamicNode)])]
  in catMaybes $ map (satisfiedDependencies $ resourceMgr broker) depsOfReverseDeps

reverseDependenciesOf :: Source -> ServiceID -> Language -> Broker -> [([Edge], DynamicNode)]
reverseDependenciesOf source serviceID language broker =
  let filterLookup dep = filterByLanguage language . DG.lookupReverseDependencies dep
      reverseProductDeps = map (productToDynamicDependency source) $ filterLookup serviceID $ productDependencies broker
      reverseDynamicDeps = filterLookup (source, serviceID) $ dynamicDependencies broker
  in reverseProductDeps ++ reverseDynamicDeps

dependenciesOf :: [([Edge], DynamicNode)] -> Language -> Broker -> [(([Edge], DynamicNode), [([Edge], DynamicNode)])]
dependenciesOf deps language broker =
  flip map deps $ \(edges, (source, serviceID)) ->
    let productDeps = map (productToDynamicDependency source) $ filterByLanguage language $ DG.lookupDependencies serviceID $ productDependencies broker
        dynamicDeps = filterByLanguage language $ DG.lookupDependencies (source, serviceID) $ dynamicDependencies broker
    in ((edges, (source, serviceID)), productDeps ++ dynamicDeps)

satisfiedDependencies :: ResourceManager -> (([Edge], DynamicNode), [([Edge], DynamicNode)]) -> Maybe Request
satisfiedDependencies rMgr ((_, (source', serviceID)), dynamicDeps') =
  let msgs = concat $ map (dynamicDependencyToMessage rMgr) dynamicDeps'
  in if foldl (\bool req -> bool || isNothing req) False msgs
    then Nothing
    else Just $ Req.Request source' serviceID $ catMaybes msgs

productToDynamicDependency :: Source -> ([Edge], ProductNode) -> ([Edge], DynamicNode)
productToDynamicDependency source (edges, serviceID) = (edges, (source, serviceID))

dynamicDependencyToMessage :: ResourceManager -> ([Edge], DynamicNode) -> [Maybe Req.Message]
dynamicDependencyToMessage rMgr (edges, (source, serviceID)) =
  if serviceID == "source"
    then [Req.SourceMessage <$> R.lookupSourceMessage source rMgr]
    else map (\(product, language) -> Req.ProductMessage <$> R.lookupProductMessage (source, serviceID, product, language) rMgr) edges

filterByLanguage :: Language -> [([Edge], a)] -> [([Edge], a)]
filterByLanguage language list =
  let list' = flip map list $ \(edges, nodes) ->
        let filteredEdges = filter (\(_, language') -> language == language') edges
        in (filteredEdges, nodes)
  in filter (\(edges, _) -> length edges /= 0) list'

registerDynamicDependency :: Source -> ServiceID -> [([Edge], DynamicNode)] -> Broker -> ([Source],Broker)
registerDynamicDependency source serviceID dependsOn broker =
  let dynamicDependencies' = DG.register (source, serviceID) dependsOn (dynamicDependencies broker)
      sources = map (\(_, (source', _)) -> source') dependsOn
      broker' = broker
        { dynamicDependencies = dynamicDependencies'
        }
  in (sources, broker')

printDependencyGraph :: Broker -> IO ()
printDependencyGraph broker =
  print (productDependencies broker)
