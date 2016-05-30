
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Monto.Broker
  ( empty
  , printBroker
  , registerService
  , deregisterService
  , registerDynamicDependency
  , unknownSources
  , printProductDependencyGraph
  , printDynamicDependencyGraph
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
import qualified Monto.DynamicDependency as DD
import           Monto.RegisterServiceRequest (RegisterServiceRequest)
import qualified Monto.RegisterServiceRequest as RQ
import           Monto.Service(Service(Service))
import qualified Monto.Service as S
import           Monto.Request (Request)
import qualified Monto.Request as Req

type Edge = (Product, Language)
type DynamicNode = (Source, ServiceID)
type ProductNode = ServiceID
type DynamicDependency = ([Edge], DynamicNode)
type ProductDependency = ([Edge], ProductNode)

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
  putStrLn "Broker services:"
  print (services broker)
  putStrLn "Broker portPool:"
  print (portPool broker)
  putStrLn "Broker productDependencyGraph:"
  printProductDependencyGraph broker
  putStrLn "Broker dynamicDependencyGraph:"
  printDynamicDependencyGraph broker

registerRequestToService :: Port -> RegisterServiceRequest -> Service
registerRequestToService port r =
  Service (RQ.serviceID r) (RQ.label r) (RQ.description r) (RQ.products r) port (RQ.options r)

dependenciesOfService :: RegisterServiceRequest -> [([Edge], ProductNode)]
dependenciesOfService = fmap productDependencyToTuple . RQ.dependencies
  where
    productDependencyToTuple (PD.ProductDependency sid prod lang) = ([(prod,lang)],sid)
    productDependencyToTuple (PD.SourceDependency lang) = ([("source",lang)],"source")

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
  in (servicesWithSatisfiedDependencies source serviceID broker', broker')

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
    in (servicesWithSatisfiedDependencies source serviceID broker', broker')

servicesWithSatisfiedDependencies :: Source -> ServiceID -> Broker -> [Request]
servicesWithSatisfiedDependencies source serviceID broker =
  let reverseDeps = reverseDependenciesOf source serviceID broker :: [DynamicDependency]
      depsOfReverseDeps = map (dependenciesOf broker) reverseDeps :: [(DynamicDependency, [DynamicDependency])]
  in mapMaybe (satisfiedDependencies $ resourceMgr broker) depsOfReverseDeps

reverseDependenciesOf :: Source -> ServiceID  -> Broker -> [DynamicDependency]
reverseDependenciesOf source serviceID broker =
  let reverseProductDeps = map (productToDynamicDependency source)
                           $ DG.lookupReverseDependencies serviceID $ productDependencies broker
      reverseDynamicDeps = DG.lookupReverseDependencies (source, serviceID) $ dynamicDependencies broker
  in reverseProductDeps ++ reverseDynamicDeps

dependenciesOf :: Broker -> DynamicDependency -> (DynamicDependency, [DynamicDependency])
dependenciesOf broker (edges, (source, serviceID)) =
  let productDeps = map (productToDynamicDependency source)
                    $ DG.lookupDependencies serviceID $ productDependencies broker
      dynamicDeps = DG.lookupDependencies (source, serviceID) $ dynamicDependencies broker
  in ((edges, (source, serviceID)), productDeps ++ dynamicDeps)

satisfiedDependencies :: ResourceManager -> (DynamicDependency, [DynamicDependency]) -> Maybe Request
satisfiedDependencies rMgr ((_, (source, serviceID)), dynamicDeps) =
  case sequence $ concatMap (dynamicDependencyToMessage rMgr) dynamicDeps of
    Nothing -> Nothing
    Just msgs -> Just $ Req.Request source serviceID msgs

productToDynamicDependency :: Source -> ProductDependency -> DynamicDependency
productToDynamicDependency source (edges, serviceID) = (edges, (source, serviceID))

dynamicDependencyToMessage :: ResourceManager -> DynamicDependency -> [Maybe Req.Message]
dynamicDependencyToMessage rMgr (edges, (source, serviceID)) =
  if serviceID == "source"
    then [Req.SourceMessage <$> R.lookupSourceMessage source rMgr]
    else map (\(product, language) ->
               Req.ProductMessage <$> R.lookupProductMessage (source, serviceID, product, language) rMgr) edges

registerDynamicDependency :: Broker -> Source -> ServiceID -> [DynamicDependency] -> Broker
registerDynamicDependency broker source serviceID deps =
   broker { dynamicDependencies = DG.register (source, serviceID) deps (dynamicDependencies broker) }

unknownSources :: Broker -> [DD.DynamicDependency] -> [Source]
unknownSources = undefined

printProductDependencyGraph :: Broker -> IO ()
printProductDependencyGraph broker =
  print (productDependencies broker)

printDynamicDependencyGraph :: Broker -> IO ()
printDynamicDependencyGraph broker =
  print (dynamicDependencies broker)