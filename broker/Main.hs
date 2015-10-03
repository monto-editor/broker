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
  broker <- newMVar $ B.empty (fromPort opts) (toPort opts)
  broker' <- readMVar broker
  let portPool = B.portPool broker'
  socketPool <- newMVar M.empty
  threads <- forM portPool $ runServiceThread opts snk socketPool broker

  let stopExcecution = putMVar interrupted Interrupted
  _ <- installHandler sigINT  (Catch stopExcecution) Nothing
  _ <- installHandler sigTERM (Catch stopExcecution) Nothing

  Z.withSocket ctx Z.Rep $ \regSocket -> do

    Z.bind regSocket (registration opts) `catch` \(e :: SomeException) -> do
      putStrLn $ unwords ["couldn't bind address", registration opts, "for registrations"]
      _ <- throw e
      putStrLn $ unwords ["listen on address", registration opts, "for registrations"]

    sourceThread <- runSourceThread opts src snk socketPool broker
    registerThread <- forkIO $ forever $ do
      rawMsg <- Z.receive regSocket
      let maybeDeregister = A.decodeStrict rawMsg :: Maybe D.DeregisterService
      let maybeRegister = A.decodeStrict rawMsg :: Maybe RQ.RegisterServiceRequest
      case maybeDeregister of
        Just deregister -> onDeregisterMessage deregister broker regSocket snk
        Nothing -> yield
      case maybeRegister of
        Just register -> onRegisterMessage register regSocket broker snk
        Nothing -> yield

    _ <- readMVar interrupted
    forM_ threads killThread
    killThread registerThread
    killThread sourceThread

runSourceThread :: Z.Receiver a => Z.Sender b => Options -> Socket a -> Socket b -> MVar SocketPool -> MVar Broker -> IO ThreadId
runSourceThread opts src snk socketPool broker =
  forkIO $ forever $ do
    rawMsg <- Z.receive src
    let maybeVersionMsg = (A.decodeStrict rawMsg) :: Maybe VersionMessage
    let maybeDiscoverMsg = (A.decodeStrict rawMsg) :: Maybe DiscoverRequest
    let maybeConfigMsg = (A.decodeStrict rawMsg) :: Maybe ConfigurationMessage
    case maybeVersionMsg of
      Just msg -> do
        when (debug opts) $ putStrLn $ unwords ["version", T.unpack (V.source msg),"->", "broker"]
        socketPool' <- readMVar socketPool
        modifyMVar_ broker $ onVersionMessage opts msg socketPool'
      Nothing -> yield
    case maybeDiscoverMsg of
      Just msg -> do
        sendDiscoverResponse snk broker $ DiscoverReq.discoverServices msg
      Nothing -> yield
    case maybeConfigMsg of
      Just msg -> do
        broker' <- readMVar broker
        let services = M.elems $ B.services broker'
        socketPool' <- readMVar socketPool
        forM_ (ConfigMsg.configureServices msg) $ \config ->
          let service = head $ filter (\s -> ConfigMsg.serviceID config == B.serviceID s) services
          in Z.send (fromJust (M.lookup (B.port service) socketPool')) []
                  $ BS.concat $ BSL.toChunks $ A.encode [config]
      Nothing -> yield

sendDiscoverResponse :: Z.Sender a => Socket a -> MVar Broker -> [ServiceDiscover] -> IO()
sendDiscoverResponse snk broker discoverFilterList= do
  Z.send snk [Z.SendMore] "discover"
  b <- readMVar broker
  Z.send snk [] $ BS.concat $ BSL.toChunks $ A.encode $ findServices discoverFilterList b

runServiceThread :: Z.Sender a => Options -> Socket a -> MVar SocketPool -> MVar Broker -> Port -> IO ThreadId
runServiceThread opts snk socketPool broker port =
  forkIO $
    Z.withContext $ \ctx ->
      Z.withSocket ctx Z.Pair $ \sckt -> do
        Z.bind sckt ("tcp://*:" ++ show port)
        putStrLn $ unwords ["listen on address", "tcp://*:" ++ show port]
        modifyMVar_ socketPool $ return . M.insert port sckt
        forever $ do
          rawMsg' <- Z.receive sckt
          broker' <- readMVar broker
          let serviceID = getServiceIdByPort port broker'
          Z.send snk [Z.SendMore] (TextEnc.encodeUtf8 serviceID)
          Z.send snk [] rawMsg'
          let msg = A.decodeStrict rawMsg'
          for_ msg $ \msg' -> do
            when (debug opts) $ putStrLn $ unwords [T.unpack serviceID, T.unpack (P.source msg'), "->", "broker"]
            socketPool' <- readMVar socketPool
            modifyMVar_ broker $ onProductMessage opts msg' socketPool'

findServices :: [ServiceDiscover] -> Broker -> [DiscoverResponse]
findServices discoverList b =
  map
    (\(B.Service serviceID' label' description' language' product' _ configuration')
      -> DiscoverResp.DiscoverResponse serviceID' label' description' language' product' configuration')
    (filter (\(B.Service serviceID' _ _ language' product' _ _)
      -> case List.length discoverList of
        0 -> True
        _ -> any (\(DiscoverReq.ServiceDiscover serviceID'' language'' product'')
          -> case serviceID'' of
            Just id' -> id' == serviceID'
            Nothing -> case language'' of
              Just lng -> case product'' of
                Just prod -> prod == product' && lng == language'
                Nothing -> lng == language'
              Nothing -> case product'' of
                Just prod -> prod == product'
                Nothing -> False)
          discoverList)
      (M.elems $ B.services b))

getServiceIdByPort :: Port -> Broker -> ServiceID
getServiceIdByPort port broker =
  B.serviceID (List.head (List.filter (\service -> B.port service == port) (M.elems (B.services broker))))

sendRegisterServiceResponse :: Z.Sender a => Socket a -> T.Text -> Maybe Int -> IO ()
sendRegisterServiceResponse socket text port =
  Z.send socket [] (BS.concat $ BSL.toChunks (A.encode (RS.RegisterServiceResponse text port)))

onRegisterMessage :: Z.Sender a => Z.Sender b => RQ.RegisterServiceRequest -> Socket a -> MVar Broker -> Socket b -> IO()
onRegisterMessage register regSocket broker snk= do
  let serviceID = RQ.serviceID register
  b <- readMVar broker
  let noFreePorts = List.length $ B.portPool b
  case noFreePorts of
    0 -> do
      putStrLn $ unwords ["register", T.unpack serviceID, "failed: no free ports"]
      sendRegisterServiceResponse regSocket "failed: no free ports" Nothing
    _ -> do
      let serviceIdExists = M.lookup serviceID (B.services b)
      case serviceIdExists of
        Just _ -> do
          putStrLn $ unwords ["register", T.unpack serviceID, "failed: service id already exists"]
          sendRegisterServiceResponse regSocket "failed: service id exists" Nothing
        Nothing -> do
          putStrLn $ unwords ["register", T.unpack serviceID, "->", "broker"]
          modifyMVar_ broker (return . B.registerService register)
          b' <- readMVar broker
          case M.lookup serviceID (B.services b') of
            Just service -> do
              sendRegisterServiceResponse regSocket "ok" $ Just $ B.port service
              sendDiscoverResponse snk broker []
            Nothing -> do
              putStrLn $ unwords ["register", T.unpack serviceID, "failed: service did not register correctly"]
              sendRegisterServiceResponse regSocket "failed: service did not register correctly" Nothing

onDeregisterMessage :: Z.Sender a => Z.Sender b => D.DeregisterService -> MVar Broker -> Socket a -> Socket b -> IO()
onDeregisterMessage deregMsg broker socket snk = do
  putStrLn $ unwords ["deregister", T.unpack (D.deregisterServiceID deregMsg), "->", "broker"]
  Z.send socket [] ""
  let serviceID = D.deregisterServiceID deregMsg

  broker' <- readMVar broker
  let maybeService = M.lookup serviceID $ B.services broker'
  case maybeService of
    Just _ -> do
      modifyMVar_ broker (return . B.deregisterService (D.deregisterServiceID deregMsg))
      sendDiscoverResponse snk broker []
    Nothing -> yield

onVersionMessage :: Options -> VersionMessage -> SocketPool -> Broker -> IO Broker
{-# INLINE onVersionMessage #-}
onVersionMessage = onMessage B.newVersion

onProductMessage :: Options -> ProductMessage -> SocketPool -> Broker -> IO Broker
{-# INLINE onProductMessage #-}
onProductMessage = onMessage B.newProduct

onMessage :: (message -> Broker -> ([Response],Broker)) -> Options -> message -> SocketPool -> Broker -> IO Broker
{-# INLINE onMessage #-}
onMessage handler opts msg socketPool broker = do
  let (responses,broker') = handler msg broker
  sendResponses opts socketPool broker responses
  return broker'

sendResponses :: Options -> SocketPool -> Broker -> [Response] -> IO ()
{-# INLINE sendResponses #-}
sendResponses opts socketPool broker = mapM_ (sendResponse opts socketPool broker)

sendResponse :: Options -> SocketPool -> Broker -> Response -> IO ()
{-# INLINE sendResponse #-}
sendResponse opts socketPool broker (B.Response _ server reqs) = do
  let response = A.encode $ A.toJSON $ map toJSON reqs
  let services = List.filter (\service -> ((B.Server (B.product service) (B.language service)) == server)) (M.elems $ B.services broker)
  forM_ services $ \service -> do
    Z.send' (fromJust $ M.lookup (B.port service) socketPool) [] response
    when (debug opts) $ putStrLn $ unwords ["broker",showReqs, "->", show server]
    where
      toJSON req = case req of
        B.VersionMessage vers -> A.toJSON vers
        B.ProductMessage prod -> A.toJSON prod
      showReqs = show $ flip map reqs $ \req ->
        case req of
          B.VersionMessage ver  -> Print $ unwords ["version",T.unpack (V.source ver)]
          B.ProductMessage prod -> Print $ concat [T.unpack (P.product prod),"/",T.unpack (P.language prod)]

data Print = Print String
instance Show Print where
  show (Print s) = s

data Interrupted = Interrupted
  deriving (Eq,Show)
