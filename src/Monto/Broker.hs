
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

import           Control.Monad

import           Data.Maybe
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as M

import           Debug.Trace

import           Monto.DynamicDependency (DynamicDependency)
import           Monto.DynamicDependency
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

data Broker = Broker
  { resourceMgr         :: ResourceManager
  , productDependencies :: DependencyGraph ServiceID [(Product,Language)]
  , dynamicDependencies :: DependencyGraph (Source, ServiceID) [(Product, Language)]
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

dependenciesOfService :: RegisterServiceRequest -> [([(Product,Language)],ServiceID)]
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
      product = "source"
      language = SM.language srcMsg
  in (servicesWithSatisfiedDependencies source serviceID product language broker', broker')

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
        product = P.product pr
        language = P.language pr
    in (servicesWithSatisfiedDependencies source serviceID product language broker', broker')

servicesWithSatisfiedDependencies :: Source -> ServiceID -> Product -> Language -> Broker -> [Request]
servicesWithSatisfiedDependencies source serviceID product language broker =
  let rMgr = resourceMgr broker
      productGraph = productDependencies broker :: DependencyGraph ServiceID [(Product, Language)]
      dynamicGraph = dynamicDependencies broker :: DependencyGraph (Source, ServiceID) [(Product, Language)]

      reverseProductDeps = filterByLanguage language $ DG.lookupReverseDependencies serviceID productGraph :: [([(Product, Language)], ServiceID)]
      reverseDynamicDeps = filterByLanguage language $ DG.lookupReverseDependencies (source, serviceID) dynamicGraph :: [([(Product, Language)], (Source, ServiceID))]

      productDepsOfReverseProductDeps = map (filterByLanguage language) $ dependenciesOf productGraph reverseProductDeps :: [[([(Product, Language)], ServiceID)]]
      dynamicDepsOfReverseDynamicDeps = map (filterByLanguage language) $ dependenciesOf dynamicGraph reverseDynamicDeps :: [[([(Product, Language)], (Source, ServiceID))]]

      productDepsOfReverseDynamicDeps = map (filterByLanguage language) $ dependenciesOf productGraph (map dynamicToProductDependency reverseDynamicDeps) :: [[([(Product, Language)], ServiceID)]]
      dynamicDepsOfReverseProductDeps = map (filterByLanguage language) $ dependenciesOf dynamicGraph (map (productToDynamicDependency source) reverseProductDeps) :: [[([(Product, Language)], (Source, ServiceID))]]

      allDepsOfReverseProductDeps = zip reverseProductDeps $ productDepsOfReverseProductDeps ++ (deepMap dynamicToProductDependency dynamicDepsOfReverseProductDeps) :: [(([(Product, Language)], ServiceID), [([(Product, Language)], ServiceID)])]
      allDepsOfReverseDynamicDeps = zip reverseDynamicDeps $ dynamicDepsOfReverseDynamicDeps ++ (deepMap (productToDynamicDependency source) productDepsOfReverseDynamicDeps) :: [(([(Product, Language)], (Source, ServiceID)), [([(Product, Language)], (Source, ServiceID))])]

      allDeps = (flip map allDepsOfReverseProductDeps $ \((list, serviceID), deps) ->
                  ((list, (source, serviceID)), (flip map deps $ \(list', serviceID') ->
                          ((list', (source, serviceID')))))) ++ allDepsOfReverseDynamicDeps
      allRequests = flip map allDeps $ \((_, (source', serviceID)), dynamicDeps') ->
        let msgs = concat $ map (dynamicDependencyToMessage rMgr) dynamicDeps'
        in if foldl (\bool req -> bool || isNothing req) False msgs
          then Nothing
          else Just $ Req.Request source' serviceID $ catMaybes msgs
  in catMaybes allRequests

filterByLanguage :: Language -> [([(Product, Language)], a)] -> [([(Product, Language)], a)]
filterByLanguage language list =
  let list' = flip map list $
        \(edges, nodes) ->
          let filteredEdges = filter (\(product, language') -> language == language') edges
          in (filteredEdges, nodes)
  in filter (\(edges, nodes) -> length edges /= 0) list'



deepMap :: (a -> b) -> [[a]] -> [[b]]
deepMap f = map (map f)

dependenciesOf :: Ord a => DependencyGraph a b -> [(b, a)] -> [[(b, a)]]
dependenciesOf graph =
  map (\(e, d) -> DG.lookupDependencies d graph)

satisfiedDependencies :: Broker -> [(Source, ServiceID)] -> Maybe Request
satisfiedDependencies broker productNodes = undefined

productToDynamicDependency :: Source -> ([(Product, Language)], ServiceID) -> ([(Product, Language)], (Source, ServiceID))
productToDynamicDependency source (edges, serviceID) = (edges, (source, serviceID))

dynamicToProductDependency :: ([(Product, Language)], (Source, ServiceID)) -> ([(Product, Language)], ServiceID)
dynamicToProductDependency (edges, (_, serviceID)) = (edges, serviceID)

productDependencyToMessage :: Source -> ResourceManager -> ([(Product, Language)], ServiceID) -> [Maybe Req.Message]
productDependencyToMessage source rMgr (edges, serviceID) =
  if serviceID == "source"
    then [Req.SourceMessage <$> R.lookupSourceMessage source rMgr]
    else map (\(product, language) -> Req.ProductMessage <$> R.lookupProductMessage (source, serviceID, product, language) rMgr) edges

dynamicDependencyToMessage :: ResourceManager -> ([(Product, Language)], (Source, ServiceID)) -> [Maybe Req.Message]
dynamicDependencyToMessage rMgr (edges, (source, serviceID)) =
  if serviceID == "source"
    then [Req.SourceMessage <$> R.lookupSourceMessage source rMgr]
    else map (\(product, language) -> Req.ProductMessage <$> R.lookupProductMessage (source, serviceID, product, language) rMgr) edges

registerDynamicDependency :: Source -> ServiceID -> [([(Product, Language)], (Source, ServiceID))] -> Broker -> ([Source],Broker)
registerDynamicDependency source serviceID dependsOn broker =
  let dynamicDependencies' = DG.register (source, serviceID) dependsOn (dynamicDependencies broker)
      sources = List.map (\(_, (source, _)) -> source) dependsOn
      broker' = broker
        { dynamicDependencies = dynamicDependencies'
        }
  in (sources, broker')

printDependencyGraph :: Broker -> IO ()
printDependencyGraph broker =
  print (productDependencies broker)
