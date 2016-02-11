
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Monto.Broker
  ( empty
  , printBroker
  , registerService
  , deregisterService
  , registerProductDependency
  , printDependencyGraph
  , Broker(..)
  , newVersion
  , newProduct
  , Service(..)
  , ServiceDependency(..)
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
import           Monto.ProductDependency (ProductDependency)
import           Monto.ResourceManager (ResourceManager)
import           Monto.ProductDescription (ProductDescription)
import qualified Monto.ProductDescription as PD
import qualified Monto.ResourceManager as R
import           Monto.DependencyGraph (DependencyGraph)
import qualified Monto.DependencyGraph as DG
import           Monto.RegisterServiceRequest (RegisterServiceRequest)
import qualified Monto.RegisterServiceRequest as RQ
import           Monto.Service(Service(Service))
import qualified Monto.Service as S
import           Monto.ServiceDependency
import           Monto.Request (Request)
import qualified Monto.Request as Req

data Broker = Broker
  { resourceMgr         :: ResourceManager
  , serviceDependencies :: DependencyGraph ServiceDependency
  , services            :: Map ServiceID Service
  , portPool            :: [Port]
  , serviceOnPort       :: Map Port ServiceID
  } deriving (Eq,Show)

empty :: Port -> Port -> Broker
{-# INLINE empty #-}
empty from to = Broker
  { resourceMgr = R.empty
  , serviceDependencies = DG.empty
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
  Service (RQ.serviceID r) (RQ.label r) (RQ.description r) (RQ.language r) (RQ.products r) port (RQ.options r)

dependencyOfService :: ServiceID -> Language -> ProductDescription -> (ServiceDependency,[ServiceDependency])
dependencyOfService sid lang desc = (ServiceDependency sid (PD.product desc) lang,PD.dependsOn desc)

registerService :: RegisterServiceRequest -> Broker -> Broker
{-# INLINE registerService #-}
registerService register broker =
  case portPool broker of
    (port:restPool) ->
        let serviceID = RQ.serviceID register
            lang = RQ.language register
            service = registerRequestToService port register
            deps = map (dependencyOfService serviceID lang) $ RQ.products register
        in broker
               { serviceDependencies =
                     foldl (\graph (from,to) -> DG.register from to graph)
                           (serviceDependencies broker)
                           deps
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
    , serviceDependencies = DG.filterDeps (\(ServiceDependency sid _ _) -> sid /= serviceID) (serviceDependencies broker)
    , serviceOnPort = M.delete (S.port service) (serviceOnPort broker)
    }

newVersion :: VersionMessage -> Broker -> ([Request],Broker)
{-# INLINE newVersion #-}
newVersion version broker =
  let broker' = broker
        { resourceMgr = snd $ R.updateVersion version $ resourceMgr broker
        }
  in (servicesWithSatisfiedDependencies (V.source version) (SourceDependency (V.language version)) broker', broker')
--error $ "TODO: Lookup service and product dependencies and notify "
--          ++ "services that have satifies dependencies"

newProduct :: ProductMessage -> Broker -> ([Request],Broker)
{-# INLINE newProduct #-}
newProduct pr broker
  | R.isOutdated pr (resourceMgr broker) = ([],broker)
  | otherwise =
    let broker' = broker
          { resourceMgr = snd $ R.updateProduct' pr $ resourceMgr broker
          }
    in (servicesWithSatisfiedDependencies (P.source pr) sdep broker', broker')
       where
         sdep = ServiceDependency (P.serviceId pr) (P.product pr) (P.language pr)
--error $ "TODO: Lookup service and product dependencies and notify "
--            ++ "services that have satifies dependencies"

servicesWithSatisfiedDependencies :: Source -> ServiceDependency -> Broker -> [Request]
servicesWithSatisfiedDependencies src d broker =
  let rdeps = DG.lookupReverseDependencies d (serviceDependencies broker)
  in flip mapMaybe rdeps $ \rdep ->
       case rdep of
         SourceDependency _ -> error "a source cannot depend on something else"
         ServiceDependency serviceID prod lang -> do
           msgs <- forM (DG.lookupDependencies rdep (serviceDependencies broker)) $ \dep ->
                     case dep of
                       SourceDependency _ -> Req.VersionMessage <$> R.lookupVersionMessage src (resourceMgr broker)
                       ServiceDependency sid prod' lang' -> Req.ProductMessage <$> R.lookupProductMessage (src,sid,prod',lang') (resourceMgr broker)
           return $ Req.Request src serviceID prod lang msgs

registerProductDependency :: ProductDependency -> [ProductDependency] -> Broker -> ([Source],Broker)
registerProductDependency product dependsOn broker =
  error $ "TODO: register a product dependency and return the sources that "
       ++ "have to be requested from the IDE"

printDependencyGraph :: Broker -> IO ()
printDependencyGraph broker =
  print (serviceDependencies broker)
