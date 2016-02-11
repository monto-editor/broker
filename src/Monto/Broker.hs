
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

import           Monto.Types
import           Monto.VersionMessage (VersionMessage)
import qualified Monto.VersionMessage as V
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
  , productDependencies :: DependencyGraph ServiceID (Product,Language)
  , services            :: Map ServiceID Service
  , portPool            :: [Port]
  , serviceOnPort       :: Map Port ServiceID
  } deriving (Eq,Show)

empty :: Port -> Port -> Broker
{-# INLINE empty #-}
empty from to = Broker
  { resourceMgr = R.empty
  , productDependencies = DG.register "source" [] DG.empty
  , services = M.empty
  , portPool = [from..to]
  , serviceOnPort = M.empty
  }

printBroker :: Broker -> IO()
printBroker broker = do
  print (services broker)
  print (portPool broker)

registerRequestToService :: Port -> RegisterServiceRequest -> Service
registerRequestToService port r =
  Service (RQ.serviceID r) (RQ.label r) (RQ.description r) (RQ.products r) port (RQ.options r)

dependenciesOfService :: RegisterServiceRequest -> [((Product,Language),ServiceID)]
dependenciesOfService = fmap productDependencyToTuple . RQ.dependencies
  where
    productDependencyToTuple (PD.ProductDependency sid prod lang) = ((prod,lang),sid)
    productDependencyToTuple (PD.SourceDependency lang) = (("source",lang),"source")

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

newVersion :: VersionMessage -> Broker -> ([Request],Broker)
{-# INLINE newVersion #-}
newVersion version broker =
  let broker' = broker
        { resourceMgr = R.updateVersion version $ resourceMgr broker
        }
  in (servicesWithSatisfiedDependencies (V.source version) "source" "source" (V.language version) broker', broker')
--error $ "TODO: Lookup service and product dependencies and notify "
--          ++ "services that have satifies dependencies"

newProduct :: ProductMessage -> Broker -> ([Request],Broker)
{-# INLINE newProduct #-}
newProduct pr broker
  | R.isOutdated pr (resourceMgr broker) = ([],broker)
  | otherwise =
    let broker' = broker
          { resourceMgr = R.updateProduct pr $ resourceMgr broker
          }
    in (servicesWithSatisfiedDependencies (P.source pr) (P.serviceID pr) (P.product pr) (P.language pr) broker', broker')
--error $ "TODO: Lookup service and product dependencies and notify "
--            ++ "services that have satifies dependencies"

servicesWithSatisfiedDependencies :: Source -> ServiceID -> Product -> Language -> Broker -> [Request]
servicesWithSatisfiedDependencies src sid prod lang broker = do
  ((prod',lang'),sid') <- DG.lookupReverseDependencies sid (productDependencies broker)
  guard (prod == prod' && lang == lang')
  let msgs = forM (DG.lookupDependencies sid' (productDependencies broker)) $ \((prod'',lang''),sid'') ->
                case sid'' of
                  ServiceID "source" -> Req.VersionMessage <$> R.lookupVersionMessage src (resourceMgr broker)
                  ServiceID _ -> Req.ProductMessage <$> R.lookupProductMessage (src,sid'',prod'',lang'') (resourceMgr broker)
  maybeToList $ Req.Request src sid' <$> msgs

registerDynamicDependency :: Source -> ServiceID -> [(Source,ServiceID,Product,Language)] -> Broker -> ([Source],Broker)
registerDynamicDependency src sid dependsOn broker =
  error $ "TODO: register a product dependency and return the sources that "
       ++ "have to be requested from the IDE"

printDependencyGraph :: Broker -> IO ()
printDependencyGraph broker =
  print (productDependencies broker)
