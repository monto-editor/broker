{-# LANGUAGE OverloadedStrings,ScopedTypeVariables #-}
module Main where

import           System.ZMQ4 (Pair,Context,Pub,Sub,Socket)
import qualified System.ZMQ4 as Z hiding (message,source)
import           System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)

--import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Exception

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Foldable (for_)
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TextEnc

import           Monto.Broker (Broker,Response)
import qualified Monto.Broker as B
import           Monto.ConfigurationMessage (ConfigurationMessage)
import qualified Monto.ConfigurationMessage as ConfigMsg
import qualified Monto.DeregisterService as D
import           Monto.DiscoverRequest (DiscoverRequest, ServiceDiscover)
import qualified Monto.DiscoverRequest as DiscoverReq
import           Monto.DiscoverResponse (DiscoverResponse)
import qualified Monto.DiscoverResponse as DiscoverResp
import           Monto.ProductMessage (ProductMessage)
import qualified Monto.ProductMessage as P
import qualified Monto.RegisterServiceRequest as RQ
import qualified Monto.RegisterServiceResponse as RS
import           Monto.Types (Port, ServiceID)
import           Monto.VersionMessage (VersionMessage)
import qualified Monto.VersionMessage as V

import           Options.Applicative

type Addr = String
type SocketPool = Map Port (Socket Pair)
type AppState = (Broker, SocketPool)

data Options = Options
  { debug         :: Bool
  , sink          :: Addr
  , source        :: Addr
  , registration  :: Addr
  , fromPort      :: Int
  , toPort        :: Int
  }

options :: Parser Options
options = Options
  <$> switch      (short 'd' <> long "debug"        <> help "print messages that are transmitted over the broker")
  <*> strOption   (short 'k' <> long "sink"         <> help "address of the sink")
  <*> strOption   (short 'c' <> long "source"       <> help "address of the source")
  <*> strOption   (short 'r' <> long "registration" <> help "address for service registration")
  <*> option auto (short 'f' <> long "servicesFrom" <> help "port from which on services can connect")
  <*> option auto (short 't' <> long "servicesTo"   <> help "port to which services can connect")

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> options)
    ( fullDesc
    <> progDesc "Monto Broker"
    )
  Z.withContext $ \ctx ->
        run opts ctx

run :: Options -> Context -> IO()
run opts ctx = do
  Z.withSocket ctx Z.Sub $ \src -> do
   Z.bind src $ source opts
   Z.subscribe src ""
   putStrLn $ unwords ["listen on address", show $ source opts, "for versions"]

   Z.withSocket ctx Z.Pub $ \snk -> do
     Z.bind snk $ sink opts
     putStrLn $ unwords ["publish all products to sink on address", show $ sink opts]
     runServer opts ctx src snk

runServer :: Options -> Context -> Socket Sub -> Socket Pub -> IO ()
runServer opts ctx src snk = do
  interrupted <- newEmptyMVar
  let broker = B.empty (fromPort opts) (toPort opts)
  appstate <- newMVar (broker, M.empty)
  threads <- forM (B.portPool broker) $ runServiceThread opts snk appstate

  let stopExcecution = putMVar interrupted Interrupted
  _ <- installHandler sigINT  (Catch stopExcecution) Nothing
  _ <- installHandler sigTERM (Catch stopExcecution) Nothing

  Z.withSocket ctx Z.Rep $ \regSocket -> do

    Z.bind regSocket (registration opts) `catch` \(e :: SomeException) -> do
      putStrLn $ unwords ["couldn't bind address", registration opts, "for registrations"]
      _ <- throw e
      putStrLn $ unwords ["listen on address", registration opts, "for registrations"]

    sourceThread <- runSourceThread opts src snk appstate
    registerThread <- forkIO $ forever $ do
      rawMsg <- Z.receive regSocket
      let maybeRegister = A.decodeStrict rawMsg :: Maybe RQ.RegisterServiceRequest
      let maybeDeregister = A.decodeStrict rawMsg :: Maybe D.DeregisterService
      case (maybeRegister, maybeDeregister) of
        (Just register, Nothing) -> modifyMVar_ appstate $ onRegisterMessage register regSocket snk
        (Nothing, Just deregister) -> modifyMVar_ appstate $ onDeregisterMessage deregister regSocket snk
        (Nothing, Nothing) -> putStrLn "message is not a register request nor a deregister message"
        (_, _) -> putStrLn "couldn't distinguish between register request and deregister message"

    _ <- readMVar interrupted
    forM_ threads killThread
    killThread registerThread
    killThread sourceThread

runSourceThread :: Z.Receiver a => Z.Sender b => Options -> Socket a -> Socket b -> MVar AppState -> IO ThreadId
runSourceThread opts src snk appstate =
  forkIO $ forever $ do
    rawMsg <- Z.receive src
    let maybeVersionMsg = (A.decodeStrict rawMsg) :: Maybe VersionMessage
    let maybeDiscoverMsg = (A.decodeStrict rawMsg) :: Maybe DiscoverRequest
    let maybeConfigMsg = (A.decodeStrict rawMsg) :: Maybe ConfigurationMessage
    case (maybeVersionMsg, maybeDiscoverMsg, maybeConfigMsg) of
      (Just msg, Nothing, Nothing) -> do
        when (debug opts) $ putStrLn $ unwords ["version", T.unpack (V.source msg),"->", "broker"]
        modifyMVar_ appstate $ onVersionMessage opts msg
      (Nothing, Just msg, Nothing) -> do
        (broker, _) <- readMVar appstate
        sendDiscoverResponse snk broker $ DiscoverReq.discoverServices msg
      (Nothing, Nothing, Just msg) -> do
        (broker, socketPool) <- readMVar appstate
        let services = M.elems $ B.services broker
        forM_ (ConfigMsg.configureServices msg) $ \config ->
          let service = head $ filter (\s -> ConfigMsg.serviceID config == B.serviceID s) services
          in Z.send (fromJust $ M.lookup (B.port service) socketPool) [] $ convertBslToBs $ A.encode [config]
      (Nothing, Nothing, Nothing) -> putStrLn "message is not a version message nor a discover request nor a config message"
      (_, _, _) -> putStrLn "couldn't distinguish between version message, discover request and config message"

sendDiscoverResponse :: Z.Sender a => Socket a -> Broker -> [ServiceDiscover] -> IO()
sendDiscoverResponse snk broker discoverFilterList= do
  Z.send snk [Z.SendMore] "discover"
  Z.send snk [] $ convertBslToBs $ A.encode $ findServices discoverFilterList broker

runServiceThread :: Z.Sender a => Options -> Socket a -> MVar AppState -> Port -> IO ThreadId
runServiceThread opts snk appstate port =
  forkIO $
    Z.withContext $ \ctx ->
      Z.withSocket ctx Z.Pair $ \sckt -> do
        Z.bind sckt ("tcp://*:" ++ show port)
        putStrLn $ unwords ["listen on address", "tcp://*:" ++ show port]
        modifyMVar_ appstate $ \(broker, socketPool) -> return (broker, M.insert port sckt socketPool)
        forever $ do
          rawMsg <- Z.receive sckt
          (broker', _) <- readMVar appstate
          let serviceID = getServiceIdByPort port broker'
          Z.send snk [Z.SendMore] (TextEnc.encodeUtf8 serviceID)
          Z.send snk [] rawMsg
          let msg = A.decodeStrict rawMsg
          for_ msg $ \msg' -> do
            when (debug opts) $ putStrLn $ unwords [T.unpack serviceID, T.unpack (P.source msg'), "->", "broker"]
            modifyMVar_ appstate $ onProductMessage opts msg'

findServices :: [ServiceDiscover] -> Broker -> [DiscoverResponse]
findServices discoverList b =
  map
    (\(B.Service serviceID' label' description' language' product' _ configuration') ->
      DiscoverResp.DiscoverResponse serviceID' label' description' language' product' configuration')
    (filter (\(B.Service serviceID' _ _ language' product' _ _) ->
      (List.length discoverList == 0) || any (\(DiscoverReq.ServiceDiscover serviceID'' language'' product'') ->
        case (serviceID'', language'', product'') of
          (Just id', _, _) -> id' == serviceID'
          (Nothing, Just lng, Just prod) -> prod == product' && lng == language'
          (Nothing, Just lng, Nothing) -> lng == language'
          (Nothing, Nothing, Just prod) -> prod == product'
          (Nothing, Nothing, Nothing) -> False)
      discoverList)
      (M.elems $ B.services b))

getServiceIdByPort :: Port -> Broker -> ServiceID
getServiceIdByPort port broker =
  fromJust $ M.lookup port $ B.serviceOnPort broker

sendRegisterServiceResponse :: Z.Sender a => Socket a -> T.Text -> Maybe Int -> IO ()
sendRegisterServiceResponse socket text port =
  Z.send socket [] $ convertBslToBs $ A.encode $ RS.RegisterServiceResponse text port

onRegisterMessage :: Z.Sender a => Z.Sender b => RQ.RegisterServiceRequest -> Socket a -> Socket b -> AppState -> IO AppState
onRegisterMessage register regSocket snk (broker, socketPool) = do
  let serviceID = RQ.serviceID register
  case List.length $ B.portPool broker of
    0 -> do
      putStrLn $ unwords ["register", T.unpack serviceID, "failed: no free ports"]
      sendRegisterServiceResponse regSocket "failed: no free ports" Nothing
      return (broker, socketPool)
    _ -> do
      let serviceIdExists = M.lookup serviceID (B.services broker)
      case serviceIdExists of
        Just _ -> do
          putStrLn $ unwords ["register", T.unpack serviceID, "failed: service id already exists"]
          sendRegisterServiceResponse regSocket "failed: service id exists" Nothing
          return (broker, socketPool)
        Nothing -> do
          putStrLn $ unwords ["register", T.unpack serviceID, "->", "broker"]
          let broker' = B.registerService register broker
          case M.lookup serviceID (B.services broker') of
            Just service -> do
              sendRegisterServiceResponse regSocket "ok" $ Just $ B.port service
              sendDiscoverResponse snk broker' []
            Nothing -> do
              putStrLn $ unwords ["register", T.unpack serviceID, "failed: service did not register correctly"]
              sendRegisterServiceResponse regSocket "failed: service did not register correctly" Nothing
          return (broker', socketPool)

onDeregisterMessage :: Z.Sender a => Z.Sender b => D.DeregisterService -> Socket a -> Socket b -> AppState -> IO AppState
onDeregisterMessage deregMsg socket snk (broker, socketPool)= do
  putStrLn $ unwords ["deregister", T.unpack (D.deregisterServiceID deregMsg), "->", "broker"]
  Z.send socket [] ""
  case M.lookup (D.deregisterServiceID deregMsg) $ B.services broker of
    Just _ -> do
      let broker' = B.deregisterService (D.deregisterServiceID deregMsg) broker
      sendDiscoverResponse snk broker' []
      return (broker', socketPool)
    Nothing -> return (broker, socketPool)

onVersionMessage :: Options -> VersionMessage -> AppState -> IO AppState
{-# INLINE onVersionMessage #-}
onVersionMessage = onMessage B.newVersion

onProductMessage :: Options -> ProductMessage -> AppState -> IO AppState
{-# INLINE onProductMessage #-}
onProductMessage = onMessage B.newProduct

onMessage :: (message -> Broker -> ([Response],Broker)) -> Options -> message -> AppState -> IO AppState
{-# INLINE onMessage #-}
onMessage handler opts msg (broker, socketpool) = do
  let (responses,broker') = handler msg broker
  sendResponses opts (broker, socketpool) responses
  return (broker', socketpool)

sendResponses :: Options -> AppState -> [Response] -> IO ()
{-# INLINE sendResponses #-}
sendResponses opts appstate = mapM_ (sendResponse opts appstate)

sendResponse :: Options -> AppState -> Response -> IO ()
{-# INLINE sendResponse #-}
sendResponse opts (broker, socketpool) (B.Response _ server reqs) = do
  let response = A.encode $ A.toJSON $ map toJSON reqs
  let services = List.filter (\service -> ((B.Server (B.product service) (B.language service)) == server)) (M.elems $ B.services broker)
  forM_ services $ \service -> do
    Z.send' (fromJust $ M.lookup (B.port service) socketpool) [] response
    when (debug opts) $ putStrLn $ unwords ["broker",showReqs, "->", show server]
    where
      toJSON req = case req of
        B.VersionMessage vers -> A.toJSON vers
        B.ProductMessage prod -> A.toJSON prod
      showReqs = show $ flip map reqs $ \req ->
        case req of
          B.VersionMessage ver  -> Print $ unwords ["version",T.unpack (V.source ver)]
          B.ProductMessage prod -> Print $ concat [T.unpack (P.product prod),"/",T.unpack (P.language prod)]

convertBslToBs :: BSL.ByteString -> BS.ByteString
convertBslToBs msg =
  BS.concat $ BSL.toChunks msg

data Print = Print String
instance Show Print where
  show (Print s) = s

data Interrupted = Interrupted
  deriving (Eq,Show)
