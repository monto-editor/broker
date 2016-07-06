{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Monto.Broker
  ( empty
  , printBroker
  , registerService
  , deregisterService
  , printProductDependencyGraph
  , printDynamicDependencyGraph
  , Broker(..)
  , newDynamicDependency
  , newVersion
  , newProduct
  , servicesWithSatisfiedDependencies
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
import qualified Data.Set as S
import           Data.Tuple (swap)

import qualified Monto.DynamicDependency as DD
import           Monto.Types
import           Monto.SourceMessage (SourceMessage)
import qualified Monto.SourceMessage as SM
import           Monto.ProductMessage (ProductMessage)
import qualified Monto.ProductMessage as P
import           Monto.ResourceManager (ResourceManager)
import qualified Monto.ProductDependency as PD
import qualified Monto.RegisterDynamicDependencies as RD
import qualified Monto.ResourceManager as R
import           Monto.DependencyGraph (DependencyGraph)
import qualified Monto.DependencyGraph as DG
import           Monto.RegisterServiceRequest (RegisterServiceRequest)
import qualified Monto.RegisterServiceRequest as RQ
import           Monto.Service(Service(Service))
import qualified Monto.Service as S
import           Monto.Request (Request)
import qualified Monto.Request as Req

type DynamicDependency = ([(Product,Language)], (Source,ServiceID))
type ProductDependency = ([(Product,Language)], ServiceID)

data Broker = Broker
  { resourceMgr         :: ResourceManager
  , productDependencies :: DependencyGraph ServiceID          [(Product,Language)]
  , dynamicDependencies :: DependencyGraph (Source,ServiceID) [(Product,Language)]
  , services            :: Map ServiceID Service
  , portPool            :: [Port]
  } deriving (Eq,Show)

empty :: Port -> Port -> Broker
{-# INLINE empty #-}
empty from to = Broker
  { resourceMgr = R.empty
  , productDependencies = DG.register "source" [] DG.empty
  , dynamicDependencies = DG.empty
  , services = M.empty
  , portPool = [from..to]
  }

printBroker :: Broker -> IO()
printBroker broker = do
  putStrLn "Services:"
  print (services broker)
  putStrLn "Port Pool:"
  print (portPool broker)
  printProductDependencyGraph broker
  printDynamicDependencyGraph broker

registerRequestToService :: Port -> RegisterServiceRequest -> Service
registerRequestToService port r =
  Service (RQ.serviceID r) (RQ.label r) (RQ.description r) (RQ.products r) port (RQ.options r)

dependenciesOfService :: RegisterServiceRequest -> [([(Product,Language)], ServiceID)]
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

newDynamicDependency :: RD.RegisterDynamicDependencies -> Broker -> (Maybe Request, Broker)
newDynamicDependency regMsg broker =
  let source = RD.source regMsg
      serviceID = RD.serviceID regMsg
      deps = toGraphTuples (RD.dependencies regMsg)
      broker' = broker 
        { dynamicDependencies = DG.register (source, serviceID) deps (dynamicDependencies broker)
        }
  in (hasSatisfiedDependencies broker' (source,serviceID), broker')

toGraphTuples :: [DD.DynamicDependency] -> [DynamicDependency]
toGraphTuples dyndeps =
  let toGraphNode dyndep' = (DD.source dyndep', DD.serviceID dyndep')
      toGraphEdge dyndep' = (DD.product dyndep', DD.language dyndep')
      insertDynamicDependency map' dyndep' = M.insertWith (++) (toGraphNode dyndep') [toGraphEdge dyndep'] map'
  in map swap $ M.assocs $ foldl insertDynamicDependency M.empty dyndeps

servicesWithSatisfiedDependencies :: Source -> ServiceID -> Broker -> [Request]
servicesWithSatisfiedDependencies source serviceID broker =
  let reverseDeps = reverseDependenciesOf source serviceID broker
  in mapMaybe (hasSatisfiedDependencies broker) reverseDeps

reverseDependenciesOf :: Source -> ServiceID  -> Broker -> [(Source,ServiceID)]
reverseDependenciesOf source serviceID broker =
  let reverseProductDeps = map (\(_,sid) -> (source,sid))
                         $ DG.lookupReverseDependencies serviceID $ productDependencies broker
      reverseDynamicDeps = map snd
                         $ DG.lookupReverseDependencies (source, serviceID) $ dynamicDependencies broker
  in reverseProductDeps ++ reverseDynamicDeps

dependenciesOf :: Broker -> (Source,ServiceID) -> [([(Product,Language)],(Source,ServiceID))]
dependenciesOf broker (source, serviceID) =
  let productDeps = map (\(edge,sid) -> (edge,(source,sid)))
                  $ DG.lookupDependencies serviceID $ productDependencies broker
      dynamicDeps = DG.lookupDependencies (source, serviceID) $ dynamicDependencies broker
  in productDeps ++ dynamicDeps

hasSatisfiedDependencies :: Broker -> (Source,ServiceID) -> Maybe Request
hasSatisfiedDependencies broker (source, serviceID) =
  let dependencies = dependenciesOf broker (source, serviceID)
  in case mapM (isSatisfied (resourceMgr broker)) dependencies of
    Nothing -> Nothing
    Just msgs -> Just $ Req.Request source serviceID $ concat msgs

isSatisfied :: ResourceManager -> ([(Product,Language)],(Source,ServiceID)) -> Maybe [Req.Message]
isSatisfied rMgr (edges, (source, serviceID)) =
  if serviceID == "source"
    then fmap (\srcmsg -> [Req.SourceMessage srcmsg]) (R.lookupSourceMessage source rMgr)
    else mapM (\(product, language) ->
      Req.ProductMessage <$> R.lookupProductMessage (source, serviceID, product, language) rMgr) edges

printProductDependencyGraph :: Broker -> IO ()
printProductDependencyGraph broker = do 
  putStrLn "Product Dependency Graph:"
  print (productDependencies broker)

printDynamicDependencyGraph :: Broker -> IO ()
printDynamicDependencyGraph broker = do
  putStrLn "Dynamic Dependency Graph:"
  print (dynamicDependencies broker)
