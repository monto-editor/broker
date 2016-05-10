{-# LANGUAGE OverloadedStrings,ScopedTypeVariables #-}
module Main where

import           System.ZMQ4 (Pair,Context,Socket,Pub)
import qualified System.ZMQ4 as Z hiding (message,source)
import           System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)

import           Control.Concurrent
import           Control.Monad

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Foldable (for_)
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TextEnc
import           Data.Tuple

import           Monto.Broker (Broker)
import qualified Monto.Broker as B
import qualified Monto.DeregisterService as D
import           Monto.DiscoverResponse (DiscoverResponse)
import qualified Monto.DiscoverResponse as DiscoverResp
import qualified Monto.DynamicDependency as DD
import           Monto.ProductMessage (ProductMessage)
import qualified Monto.ProductMessage as P
import qualified Monto.Request as Req
import           Monto.Request (Request)
import qualified Monto.Require as RQI
import qualified Monto.RegisterDynamicDependencies as RD
import qualified Monto.RegisterServiceRequest as RQ
import qualified Monto.RegisterServiceResponse as RS
import           Monto.Types
import           Monto.SourceMessage (SourceMessage)
import qualified Monto.SourceMessage as S
import           Monto.Subscription (Topic)
import qualified Monto.Subscription as Sub

import           Options.Applicative

import           Text.Printf

type Addr = String
type SocketPool = Map Port (Socket Pair)
type AppState = (Broker, SocketPool)

data Options = Options
  { debug         :: Bool
  , sink          :: Addr
  , source        :: Addr
  , registration  :: Addr
  , discovery     :: Addr
  , config        :: Addr
  , dyndep        :: Addr
  , productTopic  :: Topic
  , fromPort      :: Port
  , toPort        :: Port
  }

options :: Parser Options
options = Options
  <$> switch      (long "debug"        <> help "print messages that are transmitted over the broker")
  <*> strOption   (long "sink"         <> help "address of the sink")
  <*> strOption   (long "source"       <> help "address of the source")
  <*> strOption   (long "registration" <> help "address for service registration")
  <*> strOption   (long "discovery"    <> help "address for service discovery")
  <*> strOption   (long "config"       <> help "address for service configurations")
  <*> strOption   (long "dyndep"       <> help "address for dynamic dependency registration")
  <*> option auto (long "topic"        <> help "topic for products that are sent to sinks")
  <*> option auto (long "servicesFrom" <> help "port from which on services can connect")
  <*> option auto (long "servicesTo"   <> help "port to which services can connect")

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> options)
    ( fullDesc
    <> progDesc "Monto Broker"
    )
  Z.withContext $ \ctx ->
    Z.withSocket ctx Z.Pub $ \snk -> do
      Z.bind snk $ sink opts
      putStrLn $ unwords ["publish all products to sink on address", sink opts]
      run opts ctx snk

run :: Options -> Context -> Socket Pub -> IO ()
run opts ctx snk = do
  interrupted <- newEmptyMVar
  let stopExcecution = putMVar interrupted Interrupted
  _ <- installHandler sigINT  (Catch stopExcecution) Nothing
  _ <- installHandler sigTERM (Catch stopExcecution) Nothing

  let broker = B.empty (fromPort opts) (toPort opts)
  appstate <- newMVar (broker, M.empty)
  sourceThread <- forkIO $ runSourceThread opts ctx appstate
  registerThread <- forkIO $ runRegisterThread opts ctx appstate
  discoverThread <- forkIO $ runDiscoverThread opts ctx appstate
  dynamicDepThread <- forkIO $ runDynamicDepThread opts ctx snk appstate
  threads <- forM (B.portPool broker) $ forkIO . runServiceThread opts ctx snk appstate

  _ <- readMVar interrupted
  forM_ threads killThread
  killThread dynamicDepThread
  killThread discoverThread
  killThread registerThread
  killThread sourceThread

runSourceThread :: Options -> Context -> MVar AppState -> IO ()
runSourceThread opts ctx appstate =
  Z.withSocket ctx Z.Sub $ \src -> do
    Z.bind src $ source opts
    Z.subscribe src ""
    T.putStrLn $ T.unwords ["listen on address", T.pack (source opts), "for versions"]
    forever $ do
      rawMsg <- Z.receive src
      case A.decodeStrict rawMsg of
        Just msg -> do
          when (debug opts) $ T.putStrLn $ T.unwords [toText (S.source msg),"->", "broker"]
          modifyMVar_ appstate $ onSourceMessage opts msg
        Nothing -> putStrLn "message is not a version message"

runRegisterThread :: Options -> Context -> MVar AppState -> IO ()
runRegisterThread opts ctx appstate =
  Z.withSocket ctx Z.Rep $ \socket -> do
    Z.bind socket (registration opts)
    T.putStrLn $ T.unwords ["listen on address", T.pack (registration opts), "for registrations"]
    forever $ do
      rawMsg <- Z.receive socket
      case (A.eitherDecodeStrict rawMsg, A.eitherDecodeStrict rawMsg) of
        (Right msg, _) -> modifyMVar_ appstate $ onRegisterMessage msg socket
        (_, Right msg) -> modifyMVar_ appstate $ onDeregisterMessage msg socket
        (Left r, Left d) -> do
          printf "Couldn't parse message: %s\n%s\n%s\n" (BS.unpack rawMsg) r d
          sendRegisterServiceResponse socket "failed: service did not register correctly" Nothing

runDynamicDepThread :: Options -> Context -> Socket Pub -> MVar AppState -> IO ()
runDynamicDepThread opts ctx snk appstate =
  Z.withSocket ctx Z.Sub $ \socket -> do
    Z.bind socket $ dyndep opts
    Z.subscribe socket ""
    putStrLn $ unwords ["listen on address", dyndep opts, "for dynamic dependency registrations"]
    forever $ do
      rawMsg <- Z.receive socket
      case (A.decodeStrict rawMsg :: Maybe RD.RegisterDynamicDependencies) of
        Just msg -> do
          putStrLn $ show msg
          modifyMVar_ appstate $ onDynamicDependencyRegistration msg snk
        Nothing -> putStrLn "couldn't parse dynamic dependency registration"

runDiscoverThread :: Options -> Context -> MVar AppState -> IO ()
runDiscoverThread opts ctx appstate =
  Z.withSocket ctx Z.Rep $ \socket -> do
    Z.bind socket (discovery opts)
    T.putStrLn $ T.unwords ["listen on address", T.pack (discovery opts), "for discover requests"]
    forever $ do
      _ <- Z.receive socket
      when (debug opts) $ printf "discover request\n"
      (broker, _) <- readMVar appstate
      let services = findServices broker
      when (debug opts) $ printf "discover response: %s\n" (show services)
      Z.send socket [] $ convertBslToBs $ A.encode services

runServiceThread :: Options -> Context -> Socket Pub -> MVar AppState -> Port -> IO ()
runServiceThread opts ctx snk appstate port@(Port p) =
  Z.withSocket ctx Z.Pair $ \socket -> do
    Z.bind socket ("tcp://*:" ++ show p)
    printf "listen on address tcp://*:%d for service\n" p
    modifyMVar_ appstate $ \(broker, socketPool) -> return (broker, M.insert port socket socketPool)
    forever $ do
      rawMsg <- Z.receive socket
      (broker', _) <- readMVar appstate
      let serviceID = getServiceIdByPort port broker'
      let msg = A.decodeStrict rawMsg
      for_ msg $ \msg' -> do
        when (length (productTopic opts) > 0) $
          Z.send snk [Z.SendMore] $
            BS.unwords $ TextEnc.encodeUtf8 <$> Sub.topic msg' (productTopic opts)
        Z.send snk [] rawMsg
        when (debug opts) $ T.putStrLn $ T.concat [toText serviceID, "/", toText $ P.product msg', "/", toText $ P.language msg', " -> broker"]
        modifyMVar_ appstate $ onProductMessage opts msg'

findServices :: Broker -> [DiscoverResponse]
findServices b = do
  (B.Service serviceID label description products _ configuration) <- M.elems $ B.services b
  return $ DiscoverResp.DiscoverResponse serviceID label description products configuration

getServiceIdByPort :: Port -> Broker -> ServiceID
getServiceIdByPort port broker =
  fromJust $ M.lookup port $ B.serviceOnPort broker

sendRegisterServiceResponse :: Z.Sender a => Socket a -> T.Text -> Maybe Port -> IO ()
sendRegisterServiceResponse socket text port =
  Z.send socket [] $ convertBslToBs $ A.encode $ RS.RegisterServiceResponse text port

toGraphTuples :: [DD.DynamicDependency] -> [([(Product, Language)], (Source, ServiceID))]
toGraphTuples dyndeps =
  let toGraphNode dyndep' = (DD.source dyndep', DD.serviceID dyndep')
      toGraphEdge dyndep' = (DD.product dyndep', DD.language dyndep')
      insertDynamicDependency map' dyndep' = M.insertWith (++) (toGraphNode dyndep') [toGraphEdge dyndep'] map'
  in map swap $ M.assocs $ foldl insertDynamicDependency M.empty dyndeps

onDynamicDependencyRegistration :: RD.RegisterDynamicDependencies -> Socket Pub -> AppState -> IO AppState
onDynamicDependencyRegistration msg snk (broker, socketPool) = do
  let broker' = B.registerDynamicDependency broker (RD.source msg) (RD.serviceID msg) $ toGraphTuples $ RD.dependencies msg
  let sources = unknownSources broker (RD.serviceID msg) (RD.dependencies msg)
  if (null sources) then
    return ()
  else do
    Z.send snk [Z.SendMore] "require"
    Z.send snk [] $ convertBslToBs $ A.encode $ RQI.Require sources
  return (broker', socketPool)

onRegisterMessage :: Z.Sender a => RQ.RegisterServiceRequest -> Socket a -> AppState -> IO AppState
onRegisterMessage register regSocket (broker, socketPool) = do
  let serviceID = RQ.serviceID register
  if List.null $ B.portPool broker
  then do
    T.putStrLn $ T.unwords ["register", toText serviceID, "failed: no free ports"]
    sendRegisterServiceResponse regSocket "failed: no free ports" Nothing
    return (broker, socketPool)
  else
    if M.member serviceID (B.services broker)
    then do
      T.putStrLn $ T.unwords ["register", toText serviceID, "failed: service id already exists"]
      sendRegisterServiceResponse regSocket "failed: service id exists" Nothing
      return (broker, socketPool)
    else do
      T.putStrLn $ T.unwords ["register", toText serviceID, "->", "broker"]
      let broker' = B.registerService register broker
      case M.lookup serviceID (B.services broker') of
        Just service ->
          sendRegisterServiceResponse regSocket "ok" $ Just $ B.port service
        Nothing -> do
          T.putStrLn $ T.unwords ["register", toText serviceID, "failed: service did not register correctly"]
          sendRegisterServiceResponse regSocket "failed: service did not register correctly" Nothing
      return (broker', socketPool)

onDeregisterMessage :: Z.Sender a => D.DeregisterService -> Socket a -> AppState -> IO AppState
onDeregisterMessage deregMsg socket (broker, socketPool)= do
  T.putStrLn $ T.unwords ["deregister", toText (D.deregisterServiceID deregMsg), "->", "broker"]
  Z.send socket [] ""
  case M.lookup (D.deregisterServiceID deregMsg) $ B.services broker of
    Just _ -> do
      let broker' = B.deregisterService (D.deregisterServiceID deregMsg) broker
      return (broker', socketPool)
    Nothing -> return (broker, socketPool)

onSourceMessage :: Options -> SourceMessage -> AppState -> IO AppState
{-# INLINE onSourceMessage #-}
onSourceMessage = onMessage B.newVersion

onProductMessage :: Options -> ProductMessage -> AppState -> IO AppState
{-# INLINE onProductMessage #-}
onProductMessage = onMessage B.newProduct

onMessage :: (message -> Broker -> ([Request],Broker)) -> Options -> message -> AppState -> IO AppState
{-# INLINE onMessage #-}
onMessage handler opts msg (broker, socketpool) = do
  let (responses,broker') = handler msg broker
  sendRequests opts (broker, socketpool) responses
  return (broker', socketpool)

sendRequests :: Options -> AppState -> [Request] -> IO ()
{-# INLINE sendRequests #-}
sendRequests opts appstate = mapM_ (sendRequest opts appstate)

sendRequest :: Options -> AppState -> Request -> IO ()
{-# INLINE sendRequest #-}
sendRequest opts (broker, socketpool) req@Req.Request {Req.serviceID = sid} = do
  let encoding = A.encode req
      maybeSocket = do
        service <- M.lookup sid (B.services broker)
        M.lookup (B.port service) socketpool
  case maybeSocket of
    Just sock -> do
      Z.send' sock [] encoding
      when (debug opts) $ T.putStrLn $ T.unwords ["broker", "->", toText sid]
    Nothing -> return ()

convertBslToBs :: BSL.ByteString -> BS.ByteString
convertBslToBs msg =
  BS.concat $ BSL.toChunks msg

data Interrupted = Interrupted
  deriving (Eq,Show)
