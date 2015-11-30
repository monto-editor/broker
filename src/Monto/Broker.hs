{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Monto.Broker
  ( empty
  , printBroker
  , registerService
  , deregisterService
  , Broker(..)
  , newVersion
  , newProduct
  , Response (..)
  , Message (..)
  , Service(..)
  , ServiceDependency(..)
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
import qualified Data.Vector as Vector

import           Monto.Types
import           Monto.VersionMessage (VersionMessage)
import           Monto.ProductMessage (ProductMessage)
import           Monto.ResourceManager (ResourceManager)
import qualified Monto.ResourceManager as R
import           Monto.DependencyGraph (DependencyGraph)
import qualified Monto.DependencyGraph as DG
import           Monto.RegisterServiceRequest (RegisterServiceRequest)
import qualified Monto.RegisterServiceRequest as RQ
import           Monto.Service(Service(Service))
import qualified Monto.Service as S
import           Monto.ServiceDependency


data Message = VersionMessage VersionMessage | ProductMessage ProductMessage
  deriving (Eq,Show)

data Response = Response Source Service [Message]
  deriving (Eq,Show)

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

registerService :: RegisterServiceRequest -> Broker -> Broker
{-# INLINE registerService #-}
registerService register broker =
  case portPool broker of
    (port:restPool) ->
        let serviceID = RQ.serviceID register
            deps = Vector.toList $ RQ.dependencies register
            service = Service serviceID
                        (RQ.label register)
                        (RQ.description register)
                        (RQ.language register)
                        (RQ.product register)
                        port
                        (RQ.options register)
        in broker
               { serviceDependencies = DG.register (ServiceDependency serviceID) deps (serviceDependencies broker)
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
    , serviceDependencies = DG.deregister (ServiceDependency serviceID) (serviceDependencies broker)
    , serviceOnPort = M.delete (S.port service) (serviceOnPort broker)
    }

newVersion :: VersionMessage -> Broker -> ([Response],Broker)
{-# INLINE newVersion #-}
newVersion version broker =
  let _ = broker
        { resourceMgr = snd $ R.updateVersion version $ resourceMgr broker
        }
  in error $ "TODO: Lookup service and product dependencies and notify "
          ++ "services that have satifies dependencies"

newProduct :: ProductMessage -> Broker -> ([Response],Broker)
{-# INLINE newProduct #-}
newProduct pr broker
  | R.isOutdated pr (resourceMgr broker) = ([],broker)
  | otherwise =
    let _ = broker
          { resourceMgr = snd $ R.updateProduct' pr $ resourceMgr broker
          }
    in error $ "TODO: Lookup service and product dependencies and notify "
            ++ "services that have satifies dependencies"
