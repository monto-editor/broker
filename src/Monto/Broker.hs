{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Monto.Broker
  ( empty
  , printBroker
  , registerService
  , deregisterService
  , printProductDependencyGraph
  , printDynamicDependencyGraph
  , Broker(..)
  , newVersion
  , newProduct
  , newDynamicDependency
  , newCommandMessageDependency
  , Service(..)
  , deleteCommandMessageDependencies
  )
  where

import           Prelude                                  hiding (id, product)

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative                      hiding (empty)
#endif

import qualified Data.List                                as L
import           Data.Map                                 (Map)
import qualified Data.Map                                 as M
import           Data.Maybe
import           Data.Tuple                               (swap)

import qualified Text.Show.Pretty                         as Pr

import           Monto.CommandDescription                 (CommandDescription)
import           Monto.CommandMessage                     (CommandMessage)
import qualified Monto.CommandMessage                     as CM
import           Monto.DependencyGraph                    (DependencyGraph)
import qualified Monto.DependencyGraph                    as DG
import           Monto.DependencyGraphCommandMessages     (DependencyGraphCommandMessages)
import qualified Monto.DependencyGraphCommandMessages     as DGCM
import qualified Monto.DynamicDependency                  as DD
import qualified Monto.ProductDependency                  as PD
import           Monto.ProductMessage                     (ProductMessage)
import qualified Monto.ProductMessage                     as PM
import qualified Monto.RegisterCommandMessageDependencies as RCMD
import qualified Monto.RegisterDynamicDependencies        as RD
import           Monto.RegisterServiceRequest             (RegisterServiceRequest)
import qualified Monto.RegisterServiceRequest             as RQ
import           Monto.Request                            (Request)
import qualified Monto.Request                            as Req
import           Monto.ResourceManager                    (ResourceManager)
import qualified Monto.ResourceManager                    as R
import           Monto.Service                            (Service (Service))
import qualified Monto.Service                            as Ser
import           Monto.Source
import           Monto.SourceMessage                      (SourceMessage)
import qualified Monto.SourceMessage                      as SM
import           Monto.Types

data Broker = Broker
  { resourceMgr                :: ResourceManager
  --                                              node type          edge type
  , productDependencies        :: DependencyGraph ServiceID          [(Product,Language)]
  , dynamicDependencies        :: DependencyGraph (Source,ServiceID) [(Product,Language)]
  , commandMessageDependencies :: DependencyGraphCommandMessages
  , commandConsumers           :: Map CommandDescription [ServiceID]
  , services                   :: Map ServiceID Service
  , portPool                   :: [Port]
  } deriving (Eq,Show)

empty :: Port -> Port -> Broker
{-# INLINE empty #-}
empty from to = Broker
  { resourceMgr = R.empty
  , productDependencies = DG.register "source" [] DG.empty
  , dynamicDependencies = DG.empty
  , commandMessageDependencies = DGCM.empty
  , commandConsumers = M.empty
  , services = M.empty
  , portPool = [from..to]
  }

-- |Creates one product dependency graph node and edge for each given ProductDependency originating from a JSON.
-- These nodes and edges can be used for insertion into the product dependency graph.
toProductDependencyGraphTuples :: [PD.ProductDependency] -> [([(Product,Language)], ServiceID)]
toProductDependencyGraphTuples = fmap productDependencyToTuple
  where
    productDependencyToTuple (PD.ProductDependency sid prod lang) = ([(prod,lang)],sid)
    productDependencyToTuple (PD.SourceDependency lang) = ([("source",lang)],"source")

-- |Creates one dynamic dependency graph node and edge for each given DynamicDependency originating from a JSON.
-- These nodes and edges can be used for insertion into the dynamic dependency graph.
toDynamicDependencyGraphTuples :: [DD.DynamicDependency] -> [([(Product,Language)], (Source,ServiceID))]
toDynamicDependencyGraphTuples aesonDeps = fmap swap $ M.assocs $ foldl insertDynamicDependency M.empty aesonDeps
  where
    toGraphNode dyndep' = (DD.source dyndep', DD.serviceID dyndep')
    toGraphEdge dyndep' = (DD.product dyndep', DD.language dyndep')
    insertDynamicDependency map' dyndep' = M.insertWith (++) (toGraphNode dyndep') [toGraphEdge dyndep'] map'

toCommandMessageDependencyGraphTuples :: [DD.DynamicDependency] -> [(Source,ServiceID,Product,Language)]
toCommandMessageDependencyGraphTuples = fmap (\dynDep -> (DD.source dynDep, DD.serviceID dynDep, DD.product dynDep, DD.language dynDep))

registerRequestToService :: Port -> RegisterServiceRequest -> Service
registerRequestToService port r =
  Service (RQ.serviceID r) (RQ.label r) (RQ.description r) (RQ.products r) port (RQ.options r)

registerService :: RegisterServiceRequest -> Broker -> Broker
{-# INLINE registerService #-}
registerService register broker =
  case portPool broker of
    (port:restPool) ->
        let serviceID = RQ.serviceID register
            service = registerRequestToService port register
            deps = toProductDependencyGraphTuples $ RQ.dependencies register
            cmds = RQ.commands register
        in broker
               { productDependencies = DG.register serviceID deps (productDependencies broker)
               , services = M.insert serviceID service (services broker)
               , commandConsumers = foldl (\consumers cmd -> M.insertWith (++) cmd [serviceID] consumers) (commandConsumers broker) cmds
               , portPool = restPool
               }
    [] -> error "no more ports available"

deregisterService :: ServiceID -> Broker -> Broker
{-# INLINE deregisterService #-}
deregisterService serviceID broker = fromMaybe broker $ do
  service <- M.lookup serviceID (services broker)
  return broker
    { services = M.delete serviceID (services broker)
    , portPool = L.insert (Ser.port service) (portPool broker)
    , productDependencies = DG.deregister serviceID (productDependencies broker)
    }

newVersion :: SourceMessage -> Broker -> (([Request],[CommandMessage]),Broker)
{-# INLINE newVersion #-}
newVersion srcMsg broker =
  let broker' = broker
        { resourceMgr = R.updateSource srcMsg $ resourceMgr broker
        }
      source = SM.source srcMsg
      language = SM.language srcMsg
      serviceID = "source"
      satisfiedServiceRequests = servicesWithSatisfiedDependencies ("source",language) (source,serviceID) broker'
      (satisfiedCommandMessages, broker'') = commandMessagesWithSatisfiedDependencies (source,serviceID,"source",language) broker'
  in ((satisfiedServiceRequests, satisfiedCommandMessages), broker'')

newProduct :: ProductMessage -> Broker -> (([Request],[CommandMessage]),Broker)
{-# INLINE newProduct #-}
newProduct pr broker
  | R.isOutdated pr (resourceMgr broker) = (([],[]),broker)
  | otherwise =
    let broker' = broker
          { resourceMgr = R.updateProduct pr $ resourceMgr broker
          }
        source = PM.source pr
        language = PM.language pr
        product = PM.product pr
        serviceID = PM.serviceID pr
        satisfiedServiceRequests = servicesWithSatisfiedDependencies (product,language) (source,serviceID) broker'
        (satisfiedCommandMessages, broker'') = commandMessagesWithSatisfiedDependencies (source,serviceID,product,language) broker'
    in ((satisfiedServiceRequests, satisfiedCommandMessages), broker'')

newDynamicDependency :: RD.RegisterDynamicDependencies -> Broker -> (Maybe Request, Broker)
newDynamicDependency regMsg broker =
  let source = RD.source regMsg
      serviceID = RD.serviceID regMsg
      deps = toDynamicDependencyGraphTuples (RD.dependencies regMsg)
      broker' = broker
        { dynamicDependencies = DG.register (source, serviceID) deps (dynamicDependencies broker)
        }
  in (hasSatisfiedDependencies broker' (source,serviceID), broker')

newCommandMessageDependency :: RCMD.RegisterCommandMessageDependencies -> Broker -> (Maybe CommandMessage, Broker)
newCommandMessageDependency regMsg broker =
  let cmdMsg = RCMD.commandMessage regMsg
      broker' = broker
                  { commandMessageDependencies = DGCM.addDependency
                                                   (RCMD.commandMessage regMsg)
                                                   (toCommandMessageDependencyGraphTuples (RCMD.dependencies regMsg))
                                                   (commandMessageDependencies broker)
                  }
      maybeCmgMsg = isCommandMessageSatisfied cmdMsg (resourceMgr broker') (commandMessageDependencies broker')
  in (maybeCmgMsg, broker')

-- |Creates requests for those registered services, whose product and dynamic dependencies are fulfilled.
-- The given (Product,Language) tuple indicates, which product in which language just became available.
-- The given (Source,ServiceID) tuple is used as a starting node in two dependency graphs.
-- The starting point in the product DG is the given serviceID. The starting point in the dynamic DG is the given (source,serviceID).
-- For all other nodes in the two DGs, that depend on these two starting point nodes,
-- it is checked if also all other dependencies are fulfilled. If that is the case a request for this service gets created.
servicesWithSatisfiedDependencies :: (Product,Language) -> (Source,ServiceID) -> Broker -> [Request]
servicesWithSatisfiedDependencies (product,language) (source,serviceID) broker =
  let reverseDeps = reverseDependenciesOf (product,language) (source,serviceID) broker
  in mapMaybe (hasSatisfiedDependencies broker) reverseDeps

-- |Finds nodes in the product and dynamic DG, which depend on the given source and serviceID.
-- In the product DG only the serviceID is used the find dependencies.
-- Found dependencies in the product DG :: ServiceID get paired with the given source,
-- so that they can be put in one list with found dependencies of the dynamic DG :: (Source, ServiceID).
-- The found nodes are additionally filtered, so that they must have an adjacent edge with the given (Product,Language) tuple.
reverseDependenciesOf :: (Product,Language) -> (Source,ServiceID) -> Broker -> [(Source,ServiceID)]
reverseDependenciesOf (product,language) (source,serviceID) broker =
  let reverseProductDeps = map (\(edges,sid) -> (edges,(source,sid)))
                         $ DG.lookupReverseDependencies serviceID $ productDependencies broker
      reverseDynamicDeps = DG.lookupReverseDependencies (source, serviceID) $ dynamicDependencies broker
  in map snd $ filter (\(edges,_) -> (product,language) `elem` edges) (reverseProductDeps ++ reverseDynamicDeps)

-- Returns Just Request for the given node, when all dependencies of the node are fulfilled,
-- otherwise Nothing.
hasSatisfiedDependencies :: Broker -> (Source,ServiceID) -> Maybe Request
hasSatisfiedDependencies broker (source, serviceID) =
  let dependencies = dependenciesOf broker (source, serviceID)
  in case mapM (isSatisfied (resourceMgr broker)) dependencies of
    Nothing -> Nothing
    Just msgs -> Just $ Req.Request source serviceID $ concat msgs

-- |Finds all dependencies of the given node :: (Source,ServiceID). One dependency is represented by the dependent node :: (Source,ServiceID)
-- and the edge which creates the dependency :: [(Product,Language)].
-- In the product DG only the serviceID is used the find dependencies.
dependenciesOf :: Broker -> (Source,ServiceID) -> [([(Product,Language)],(Source,ServiceID))]
dependenciesOf broker (source, serviceID) =
  let productDeps = map (\(edge,sid) -> (edge,(source,sid)))
                  $ DG.lookupDependencies serviceID $ productDependencies broker
      dynamicDeps = DG.lookupDependencies (source, serviceID) $ dynamicDependencies broker
  in productDeps ++ dynamicDeps

-- |Retrieves given dependencies for the requirements field of Request, but only if they are cached in the ResourceManager.
-- ProductMessages as well as SourceMessages are retrieved.
isSatisfied :: ResourceManager -> ([(Product,Language)],(Source,ServiceID)) -> Maybe [Req.Message]
isSatisfied rMgr (edges, (source, serviceID)) =
  if serviceID == "source"
    then fmap (\srcmsg -> [Req.SourceMessage srcmsg]) (R.lookupSourceMessage source rMgr)
    else mapM (\(product, language) ->
      Req.ProductMessage <$> R.lookupProductMessage (source, serviceID, product, language) rMgr) edges

-- |Returns CommandMessages, whose dependencies are fulfilled and also attached in the requirements field.
-- The given (Source,ServiceID,Product,Language) tuple indicates, which source or product just became available,
-- and is used to find a set of CommandMessages, whose dependencies could all be satisfied.
-- All these CommandMessages are then passed to isCommandMessageSatisfied to checked, if all dependencies are really satisfied.
-- CommandMessages, that are completly satisfied, get deleted from the dependency graph and an updated broker is returned.
commandMessagesWithSatisfiedDependencies :: (Source,ServiceID,Product,Language) -> Broker -> ([CommandMessage], Broker)
commandMessagesWithSatisfiedDependencies newSatisfiedDep broker =
  let graph = commandMessageDependencies broker
      cmdMsgs = catMaybes $ (\possiblySatisfiedCmdMsg ->
                  isCommandMessageSatisfied possiblySatisfiedCmdMsg (resourceMgr broker) graph)
                    <$> DGCM.lookupDependencyCommandMessages newSatisfiedDep graph
      broker' = deleteCommandMessageDependencies cmdMsgs broker
  in (cmdMsgs, broker')

-- |Looks up all dependencies of the given CommandMessage and checks if they are cached in the ResourceManager. If so, the
-- requirements field of the given CommandMessage is overridden with the specified dependencies and it is returned as a Just.
-- If at least one requirement is missing, Nothing is returned.
isCommandMessageSatisfied :: CommandMessage -> ResourceManager -> DependencyGraphCommandMessages -> Maybe CommandMessage
isCommandMessageSatisfied cmdMsg resMgr graph =
  (\requirements -> cmdMsg { CM.requirements = requirements })
    <$> mapM (\(source,serviceID,product,language) ->
               if serviceID == "source"
                 then Req.SourceMessage  <$> R.lookupSourceMessage source resMgr
                 else Req.ProductMessage <$> R.lookupProductMessage (source,serviceID,product,language) resMgr
             ) (DGCM.lookupCommandMessageDependencies cmdMsg graph)

-- |Deletes the given CommandMessages from the CommandMessage dependency graph.
-- This function should be called everytime CommandMessages are sent out the services, meaning
-- their dependencies were fulfilled.
deleteCommandMessageDependencies :: Foldable f => f CommandMessage -> Broker -> Broker
deleteCommandMessageDependencies cmdMsgs broker =
  broker
    { commandMessageDependencies = foldr DGCM.removeCommandMessage (commandMessageDependencies broker) cmdMsgs }


printBroker :: Broker -> IO()
printBroker broker = do
  putStrLn "Services:"
  putStrLn $ Pr.ppShow (services broker)
  putStrLn "Port Pool:"
  putStrLn $ Pr.ppShow (portPool broker)
  R.printResourceManager $ resourceMgr broker
  printProductDependencyGraph broker
  printDynamicDependencyGraph broker
  printCommandMessageDependencyMaps broker

printProductDependencyGraph :: Broker -> IO ()
printProductDependencyGraph broker = do
  putStrLn "Product Dependency Graph:"
  putStrLn $ Pr.ppShow (productDependencies broker)

printDynamicDependencyGraph :: Broker -> IO ()
printDynamicDependencyGraph broker = do
  putStrLn "Dynamic Dependency Graph:"
  putStrLn $ Pr.ppShow (dynamicDependencies broker)

printCommandMessageDependencyMaps :: Broker -> IO ()
printCommandMessageDependencyMaps broker = do
  putStrLn "CommandMessage Dependency Graph:"
  putStrLn $ Pr.ppShow (commandMessageDependencies broker)
