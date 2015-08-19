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
import qualified Data.Vector as Vector

import           Monto.Broker (Broker,Response,Server)
import qualified Monto.Broker as B
import qualified Monto.DeregisterService as D
import           Monto.ProductMessage (ProductMessage)
import qualified Monto.ProductMessage as P
import qualified Monto.RegisterServiceRequest as RQ
import qualified Monto.RegisterServiceResponse as RS
import           Monto.Types (ServiceID)
import           Monto.VersionMessage (VersionMessage)
import qualified Monto.VersionMessage as V

import           Options.Applicative

type Addr = String
type Sockets = Map Server (Socket Pair)

data Options = Options
  { debug         :: Bool
  , sink          :: Addr
  , source        :: Addr
  , registration  :: Addr
  }

options :: Parser Options
options = Options
  <$> switch      (long "debug"        <> help "print messages that are transmitted over the broker")
  <*> strOption   (long "sink"         <> help "address of the sink")
  <*> strOption   (long "source"       <> help "address of the source")
  <*> strOption   (long "registration" <> help "address for service registration")

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
  threads <- newMVar M.empty
  broker <- newMVar B.empty
  sockets <- newMVar M.empty

  let stopExcecution = putMVar interrupted Interrupted
  _ <- installHandler sigINT  (Catch stopExcecution) Nothing
  _ <- installHandler sigTERM (Catch stopExcecution) Nothing

  Z.withSocket ctx Z.Rep $ \regSocket -> do

    Z.bind regSocket (registration opts) `catch` \(e :: SomeException) -> do
      putStrLn $ unwords ["couldn't bind address", (registration opts), "for registrations"]
      throw e
    putStrLn $ unwords ["listen on address", (registration opts), "for registrations"]

    sourceThread <- runSourceThread opts src sockets broker
    registerThread <- forkIO $ forever $ do
      rawMsg <- Z.receive regSocket
      let maybeDeregister = (A.decodeStrict rawMsg) :: Maybe D.DeregisterService
      let maybeRegister = (A.decodeStrict rawMsg) :: Maybe RQ.RegisterServiceRequest
      case maybeDeregister of
        Just deregister -> onDeregisterMessage deregister broker regSocket sockets threads
        Nothing -> yield
      case maybeRegister of
        Just register -> do
          let serviceID = RQ.serviceID register
          let server = B.Server (RQ.product register) (RQ.language register)
          putStrLn $ unwords ["register", T.unpack serviceID, "->", "broker"]
          modifyMVar_ broker (B.registerService server serviceID (map read $ Vector.toList $ fromJust $ RQ.dependencies register))
          b <- readMVar broker
          let service = fromJust $ M.lookup serviceID (B.services b)

          thread <- runServiceThread opts snk service sockets broker
          modifyMVar_ threads $ mapInsert serviceID thread
          Z.send regSocket [] (BS.concat $ BSL.toChunks (A.encode (RS.RegisterServiceResponse "ok" $ Just $ B.port service)))
        Nothing -> yield

    _ <- readMVar interrupted
    threads' <- readMVar threads
    let elems = M.elems threads'
    forM_ elems killThread
    killThread registerThread
    killThread sourceThread

runSourceThread :: Z.Receiver a => Options -> Socket a -> MVar Sockets -> MVar Broker -> IO ThreadId
runSourceThread opts src sockets broker =
  forkIO $ forever $ do
    msg <- A.decodeStrict <$> Z.receive src
    for_ msg $ \msg' -> do
      when (debug opts) $ putStrLn $ unwords ["version", T.unpack (V.source msg'),"->", "broker"]
      sockets' <- readMVar sockets
      modifyMVar_ broker $ onVersionMessage opts msg' sockets'

runServiceThread :: Z.Sender a => Options -> Socket a -> B.Service -> MVar Sockets -> MVar Broker -> IO ThreadId
runServiceThread opts snk service sockets broker =
  forkIO $
    Z.withContext $ \ctx -> do
      Z.withSocket ctx Z.Pair $ \sckt -> do
        let port = (B.port service)
            server = (B.server service)
        Z.connect sckt ("tcp://127.0.0.1:" ++ show port)
        putStrLn $ unwords ["listen on address", "tcp://*:" ++ show port, "for", show server]
        modifyMVar_ sockets $ mapInsert server sckt
        forever $ do
          rawMsg' <- Z.receive sckt
          Z.send snk [] rawMsg'
          let msg = A.decodeStrict rawMsg'
          for_ msg $ \msg' -> do
            when (debug opts) $ putStrLn $ unwords [show server, T.unpack (P.source msg'), "->", "broker"]
            sockets' <- readMVar sockets
            modifyMVar_ broker $ onProductMessage opts msg' sockets'

onDeregisterMessage :: Z.Sender a => D.DeregisterService -> MVar Broker -> Socket a -> MVar Sockets -> MVar (Map ServiceID ThreadId) -> IO()
onDeregisterMessage deregMsg broker socket sockets threads = do
  putStrLn $ unwords ["deregister", T.unpack (D.deregisterServiceID deregMsg), "->", "broker"]
  modifyMVar_ broker (B.deregisterService (D.deregisterServiceID deregMsg))
  Z.send socket [] ""
  let serviceID = D.deregisterServiceID deregMsg

  broker' <- readMVar broker
  let maybeService = M.lookup serviceID $ B.services broker'
  case maybeService of
    Just service -> do
      let server = B.server service
      let port = B.port service

      sockets' <- readMVar sockets
      let socket' = fromJust (M.lookup server sockets')
      putStrLn $ "tcp://*:" ++ show port
      Z.close socket'
      modifyMVar_ sockets $ mapRemove server

      threads' <- readMVar threads
      let thread = fromJust $ M.lookup serviceID threads'
      killThread thread
      modifyMVar_ threads $ mapRemove serviceID

    Nothing -> yield

mapInsert :: Ord k => k -> v -> Map k v -> IO (Map k v)
mapInsert k v m = do
  return (M.insert k v m)

mapRemove :: Ord k => k -> Map k v -> IO (Map k v)
mapRemove k m = do
  return (M.delete k m)


listInsert :: Ord a => a -> [a] -> IO [a]
listInsert item list = do
  return (List.insert item list)

onVersionMessage :: Options -> VersionMessage -> Sockets -> Broker -> IO Broker
{-# INLINE onVersionMessage #-}
onVersionMessage = onMessage B.newVersion

onProductMessage :: Options -> ProductMessage -> Sockets -> Broker -> IO Broker
{-# INLINE onProductMessage #-}
onProductMessage = onMessage B.newProduct

onMessage :: (message -> Broker -> ([Response],Broker)) -> Options -> message -> Sockets -> Broker -> IO Broker
{-# INLINE onMessage #-}
onMessage handler opts msg sockets broker = do
  let (responses,broker') = handler msg broker
  sendResponses opts sockets responses
  return broker'

sendResponses :: Options -> Sockets -> [Response] -> IO ()
{-# INLINE sendResponses #-}
sendResponses opts sockets = mapM_ (sendResponse opts sockets)

sendResponse :: Options -> Sockets -> Response -> IO ()
{-# INLINE sendResponse #-}
sendResponse opts sockets (B.Response _ server reqs) = do
  let response = A.encode $ A.toJSON $ map toJSON reqs
  Z.send' (sockets M.! server) [] response
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