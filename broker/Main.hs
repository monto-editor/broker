{-# LANGUAGE OverloadedStrings,ScopedTypeVariables #-}
module Main where

import           System.ZMQ4 (Pair,Context,Socket)
import qualified System.ZMQ4 as Z hiding (message,source)
import           System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Tuple (swap)

import           Monto.Broker (Broker)
import qualified Monto.Broker as B
import qualified Monto.ResourceManager as R
import           Monto.ConfigurationMessage (ConfigurationMessage(..))
import qualified Monto.DeregisterService as D
import           Monto.DiscoverResponse (DiscoverResponse)
import qualified Monto.DiscoverResponse as DiscoverResp
import qualified Monto.DynamicDependency as DD
import qualified Monto.ProductMessage as P
import qualified Monto.Request as Req
import           Monto.Request (Request)
import qualified Monto.RegisterDynamicDependencies as RD
import qualified Monto.RegisterServiceRequest as RQ
import qualified Monto.RegisterServiceResponse as RS
import           Monto.Types
import qualified Monto.SourceMessage as S
import qualified Monto.MessagesIDE as IDE
import qualified Monto.MessagesService as Service
import qualified Monto.Service as S

import           Options.Applicative

import           Text.Printf

type Addr = String
type SocketPool = Map Port (Socket Pair)
type AppState = (Broker, SocketPool)

data Options = Options
  { debug         :: Bool
  , debugGraphs   :: Bool
  , sink          :: Addr
  , source        :: Addr
  , registration  :: Addr
  , fromPort      :: Port
  , toPort        :: Port
  }

options :: Parser Options
options = Options
  <$> switch      (long "debug"        <> help "print messages that are transmitted over the broker")
  <*> switch      (long "debugGraphs"  <> help "print dependency graphs on change")
  <*> strOption   (long "sink"         <> help "address of the sink")
  <*> strOption   (long "source"       <> help "address of the source")
  <*> strOption   (long "registration" <> help "address for service registration")
  <*> option auto (long "servicesFrom" <> help "port from which on services can connect")
  <*> option auto (long "servicesTo"   <> help "port to which services can connect")

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> options)
    ( fullDesc
    <> progDesc "Monto Broker"
    )
  Z.withContext $ \ctx ->
    Z.withSocket ctx Z.Pair $ \snk -> do
      Z.bind snk $ sink opts
      interrupted <- newEmptyMVar
      let stopExcecution = putMVar interrupted Interrupted
      _ <- installHandler sigINT  (Catch stopExcecution) Nothing
      _ <- installHandler sigTERM (Catch stopExcecution) Nothing

      let broker = B.empty (fromPort opts) (toPort opts)

      printf "Receive messages from IDE on %s and send to %s\n" (source opts) (sink opts)

      when (debugGraphs opts) $ do
        B.printProductDependencyGraph broker
        B.printDynamicDependencyGraph broker

      appstate <- newMVar (broker, M.empty)
      ideThread <- forkIO $ runIDEThread opts ctx appstate snk
      registerThread <- forkIO $ runRegisterThread opts ctx appstate
      threads <- forM (B.portPool broker) $ forkIO . runServiceThread opts ctx snk appstate

      _ <- readMVar interrupted
      forM_ threads killThread
      killThread registerThread
      killThread ideThread

runIDEThread :: Options -> Context -> MVar AppState -> Socket Pair -> IO ()
runIDEThread opts ctx appstate snk =
  Z.withSocket ctx Z.Pair $ \src -> do
    Z.bind src $ source opts
    forever $ do
      rawMsg <- Z.receive src
      case A.eitherDecodeStrict rawMsg of
        Right (IDE.SourceMessage msg) -> do
          when (debug opts) $ T.putStrLn $ T.unwords [toText (S.source msg),"->", "broker"]
          modifyMVar_ appstate $ onSourceMessage msg
        Right (IDE.ConfigurationMessages msgs) ->
          withMVar appstate $ \state ->
            forM_ msgs $ \c@(ConfigurationMessage sid conf) ->
              sendToService sid (A.encode (Service.ConfigurationMessage c)) state
        Right (IDE.DiscoverRequest request) -> do
          when (debug opts) $ printf "discover request: %s\n" (show request)
          services <- findServices <$> getBroker appstate
          -- when (debug opts) $ printf "discover response: %s\n" (show services)
          when (debug opts) $ printf "sending discover response\n"
          Z.send snk [] $ convertBslToBs $ A.encode (IDE.DiscoverResponse services)
        Left err -> printf "Coundn't parse this message from IDE: %s\nBecause %s\n" (show rawMsg) err
  where
    findServices :: Broker -> [DiscoverResponse]
    findServices b = do
      (B.Service sid label description products _ configuration) <- M.elems $ B.services b
      return $ DiscoverResp.DiscoverResponse sid label description products configuration

    onSourceMessage = onMessage B.newVersion


runRegisterThread :: Options -> Context -> MVar AppState -> IO ()
runRegisterThread opts ctx appstate =
  Z.withSocket ctx Z.Rep $ \socket -> do
    Z.bind socket (registration opts)
    T.putStrLn $ T.unwords ["listen on address", T.pack (registration opts), "for registrations"]
    forever $ do
      rawMsg <- Z.receive socket
      case (A.eitherDecodeStrict rawMsg, A.eitherDecodeStrict rawMsg) of
        (Right msg, _) -> modifyMVar_ appstate $ onRegisterMessage opts msg socket
        (_, Right msg) -> modifyMVar_ appstate $ onDeregisterMessage opts msg socket
        (Left r, Left d) -> do
          printf "Couldn't parse this message: %s\nBecause %s\nBecause %s\n" (BS.unpack rawMsg) r d
          sendRegisterServiceResponse socket "failed: service did not register correctly" Nothing

sendRegisterServiceResponse :: Z.Sender a => Socket a -> T.Text -> Maybe Port -> IO ()
sendRegisterServiceResponse socket text port =
  Z.send socket [] $ convertBslToBs $ A.encode $ RS.RegisterServiceResponse text port

toGraphTuples :: [DD.DynamicDependency] -> [([(Product, Language)], (Source, ServiceID))]
toGraphTuples dyndeps =
  let toGraphNode dyndep' = (DD.source dyndep', DD.serviceID dyndep')
      toGraphEdge dyndep' = (DD.product dyndep', DD.language dyndep')
      insertDynamicDependency map' dyndep' = M.insertWith (++) (toGraphNode dyndep') [toGraphEdge dyndep'] map'
  in map swap $ M.assocs $ foldl insertDynamicDependency M.empty dyndeps

onDynamicDependencyRegistration :: Options -> RD.RegisterDynamicDependencies -> Socket Pair -> AppState -> IO AppState
onDynamicDependencyRegistration opts msg snk (broker, socketPool) = do
  let broker' = B.registerDynamicDependency (RD.source msg) (RD.serviceID msg) (toGraphTuples $ RD.dependencies msg) broker
  when (debugGraphs opts) $ B.printDynamicDependencyGraph broker'

  -- (RD.dependencies msg) is of type [DD.DynamicDependency], but R.missingSources needs [Source]
  let missingSources = R.missingSources (map (\oneDynDep -> DD.source oneDynDep) (RD.dependencies msg)) (B.resourceMgr broker)

  when (debug opts) $ do
    putStrLn $ unwords ["requested:", show $ (map (\oneDynDep -> DD.source oneDynDep) (RD.dependencies msg))]
    putStrLn $ unwords ["present:", show $ M.keys $ R.sources $ B.resourceMgr broker]
    putStrLn $ unwords ["missing:", show $ missingSources]

  when (null missingSources) $ do
    let newlySatisfiedRequests = B.servicesWithSatisfiedDependencies (RD.source msg) (RD.serviceID msg) broker'
    forM_ newlySatisfiedRequests $ \newlySatisfiedRequest -> do
      sendToService (Req.serviceID newlySatisfiedRequest) (A.encode (Service.Request newlySatisfiedRequest)) (broker',socketPool)

    when (debug opts) $ do
      putStrLn $ unwords ["newlySatisfiedRequests", show newlySatisfiedRequests]

  return (broker', socketPool)

onRegisterMessage :: Z.Sender a => Options -> RQ.RegisterServiceRequest -> Socket a -> AppState -> IO AppState
onRegisterMessage opts register regSocket (broker, socketPool) = do
  let sid = RQ.serviceID register
  if List.null $ B.portPool broker
  then do
    T.putStrLn $ T.unwords ["register", toText sid, "failed: no free ports"]
    sendRegisterServiceResponse regSocket "failed: no free ports" Nothing
    return (broker, socketPool)
  else
    if M.member sid (B.services broker)
    then do
      T.putStrLn $ T.unwords ["register", toText sid, "failed: service id already exists"]
      sendRegisterServiceResponse regSocket "failed: service id exists" Nothing
      return (broker, socketPool)
    else do
      T.putStrLn $ T.unwords ["register", toText sid, "->", "broker"]
      let broker' = B.registerService register broker
      when (debugGraphs opts) $ B.printProductDependencyGraph broker'
      case M.lookup sid (B.services broker') of
        Just service ->
          sendRegisterServiceResponse regSocket "ok" $ Just $ B.port service
        Nothing -> do
          T.putStrLn $ T.unwords ["register", toText sid, "failed: service did not register correctly"]
          sendRegisterServiceResponse regSocket "failed: service did not register correctly" Nothing
      return (broker', socketPool)

onDeregisterMessage :: Z.Sender a => Options -> D.DeregisterService -> Socket a -> AppState -> IO AppState
onDeregisterMessage opts deregMsg socket (broker, socketPool)= do
  T.putStrLn $ T.unwords ["deregister", toText (D.deregisterServiceID deregMsg), "->", "broker"]
  Z.send socket [] ""
  case M.lookup (D.deregisterServiceID deregMsg) $ B.services broker of
    Just _ -> do
      let broker' = B.deregisterService (D.deregisterServiceID deregMsg) broker
      when (debugGraphs opts) $ B.printProductDependencyGraph broker'
      return (broker', socketPool)
    Nothing -> return (broker, socketPool)

runServiceThread :: Options -> Context -> Socket Pair -> MVar AppState -> Port -> IO ()
runServiceThread opts ctx snk appstate port@(Port p) =
  Z.withSocket ctx Z.Pair $ \serviceSocket -> do
    Z.bind serviceSocket ("tcp://*:" ++ show p)
    printf "listen on address tcp://*:%d for service\n" p
    modifyMVar_ appstate $ \(broker, socketPool) -> return (broker, M.insert port serviceSocket socketPool)
    forever $ do        
      rawMsg <- Z.receive serviceSocket
      case A.eitherDecodeStrict rawMsg of
        Right (Service.ProductMessage msg) -> do
          Z.send snk [] $ convertBslToBs (A.encode (IDE.ProductMessage msg))
          when (debug opts) $ T.putStrLn $ T.concat [toText $ P.product msg, "/", toText $ P.language msg, " -> broker"]
          modifyMVar_ appstate $ onProductMessage msg
        Right (Service.DynamicDependency msg) -> do
          when (debug opts) $ putStrLn $ show msg
          modifyMVar_ appstate $ onDynamicDependencyRegistration opts msg snk
        Left err -> printf "Couldn't parse this message from service: %s\nBecause %s\n" (show rawMsg) err
  where
    onProductMessage = onMessage B.newProduct

maybeT :: Monad m => Maybe a -> MaybeT m a
maybeT = MaybeT . return
                   
getBroker :: MVar AppState -> IO Broker
getBroker = fmap fst . readMVar

sendToService :: ServiceID -> BSL.ByteString -> AppState -> IO ()
sendToService sid msg (broker,pool) = void $ runMaybeT $ do
  port <- maybeT $ S.port <$> M.lookup sid (B.services broker)
  socket <- maybeT $ M.lookup port pool
  lift $ Z.send socket [] $ convertBslToBs msg

onMessage :: Foldable f => (message -> Broker -> (f Request,Broker)) -> message -> AppState -> IO AppState
{-# INLINE onMessage #-}
onMessage handler msg (broker,pool) = do
  let (requests,broker') = handler msg broker
  forM_ requests $ \request -> do
    printf "broker -> %s\n" (show (Req.serviceID request))
    sendToService (Req.serviceID request) (A.encode (Service.Request request)) (broker',pool)
  return (broker', pool)

convertBslToBs :: BSL.ByteString -> BS.ByteString
convertBslToBs msg =
  BS.concat $ BSL.toChunks msg

data Interrupted = Interrupted
  deriving (Eq,Show)
