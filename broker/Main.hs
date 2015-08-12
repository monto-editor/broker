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
--import           Data.Either
import           Data.Foldable (for_)
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as Vector
--import qualified Data.Text.Encoding as TE

import           Monto.Broker (Broker,Response,Server)
import qualified Monto.Broker as B
import           Monto.VersionMessage (VersionMessage)
import           Monto.ProductMessage (ProductMessage)
import qualified Monto.VersionMessage as V
import qualified Monto.ProductMessage as P
import qualified Monto.RegisterServiceRequest as RQ
import qualified Monto.RegisterServiceResponse as RS
import qualified Monto.DeregisterService as D

import qualified Network.WebSockets as WS

import           Options.Applicative

type Addr = String

type MontoSrc = Either WS.Connection (Socket Sub)
type MontoSnk = Either WS.Connection (Socket Pub)

data Options = Options
  { debug         :: Bool
  , websocket     :: Bool
  , sink          :: Addr
  , source        :: Addr
  , registration  :: Addr
  }

options :: Parser Options
options = Options
  <$> switch      (long "debug"        <> help "print messages that are transmitted over the broker")
  <*> switch      (long "websocket"    <> help "use websockets instead of zeromq")
  <*> strOption   (long "sink"         <> help "address of the sink")
  <*> strOption   (long "source"       <> help "address of the source")
  <*> strOption   (long "registration" <> help "address for service registration")

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> options)
    ( fullDesc
    <> progDesc "Monto Broker"
    )
  Z.withContext $ \ctx -> do
    if websocket opts
      then
        runWithWS opts ctx
      else
        runWithZMQ opts ctx

runWithWS :: Options -> Context -> IO()
runWithWS opts ctx = do
  interrupted <- newEmptyMVar
  WS.runServer "127.0.0.1" (read $ source opts :: Int) $ \srcSock -> do
    src <- WS.acceptRequest srcSock
    putStrLn $ unwords ["listen on address", show $ source opts, "for versions"]

    WS.runServer "127.0.0.1" (read $ sink opts :: Int) $ \snkSock -> do
      snk <- WS.acceptRequest snkSock
      putStrLn $ unwords ["publish all products to sink on address", show $ sink opts]
      runServer opts ctx (Left src) (Left snk) interrupted

runWithZMQ :: Options -> Context -> IO()
runWithZMQ opts ctx = do
  interrupted <- newEmptyMVar
  Z.withSocket ctx Z.Sub $ \src -> do
   Z.bind src $ source opts
   Z.subscribe src ""
   putStrLn $ unwords ["listen on address", show $ source opts, "for versions"]

   let stopExcecution = putMVar interrupted Interrupted
   _ <- installHandler sigINT  (Catch stopExcecution) Nothing
   _ <- installHandler sigTERM (Catch stopExcecution) Nothing

   Z.withSocket ctx Z.Pub $ \snk -> do
     Z.bind snk $ sink opts
     putStrLn $ unwords ["publish all products to sink on address", show $ sink opts]
     runServer opts ctx (Right src) (Right snk) interrupted

runServer :: Options -> Context -> MontoSrc -> MontoSnk -> MVar t -> IO ()
runServer opts ctx src snk interrupted = do
--    let threads = []
  broker <- newMVar B.empty
  sockets <- newMVar M.empty
  services <- newMVar M.empty
  Z.withSocket ctx Z.Rep $ \regSocket -> do

    Z.bind regSocket (registration opts) `catch` \(e :: SomeException) -> do
      putStrLn $ unwords ["couldn't bind address", (registration opts), "for registrations"]
      throw e
    putStrLn $ unwords ["listen on address", (registration opts), "for registrations"]

    sourceThread <- runSourceIO opts src sockets broker
    registerThread <- forkIO $ forever $ do
      rawMsg <- Z.receive regSocket
      let d = (A.decodeStrict rawMsg) :: Maybe D.DeregisterService
      let r = (A.decodeStrict rawMsg) :: Maybe RQ.RegisterServiceRequest
      case d of
        Just fromD -> onDeregisterMessage fromD broker regSocket
        Nothing -> yield
      case r of
        Just fromR -> do
          let serviceID = RQ.registerServiceID fromR
          let server = B.Server (RQ.product fromR) (RQ.language fromR)
          putStrLn $ unwords ["register", T.unpack serviceID, "->", "broker"]
          modifyMVar_ broker (B.registerService server serviceID)
          modifyMVar_ broker (B.registerServer server (map read (Vector.toList $ fromJust (RQ.dependencies fromR))))
          b <- readMVar broker
          let port = B.port $ head (List.filter (\(B.Service _ serviceID' _) -> serviceID' == serviceID) (B.servers b))

          _ <- forkIO $
            Z.withSocket ctx Z.Pair $ \sckt -> do
              Z.bind sckt ("tcp://*:" ++ show port) `catch` \(e :: SomeException) -> do
                putStrLn $ unwords ["couldn't bind address", "tcp://*:" ++ show port, "for server", show server]
                throw e
              putStrLn $ unwords ["listen on address", "tcp://*:" ++ show port, "for", show server]
              modifyMVar_ sockets $ myInsert server sckt
              modifyMVar_ services $ myInsert serviceID sckt
          --      List.insert threads thread
              forever $ do
                rawMsg' <- Z.receive sckt
                either (sendToWS rawMsg') (sendToZMQ rawMsg') snk
                let msg = A.decodeStrict rawMsg'
                for_ msg $ \msg' -> do
                  when (debug opts) $ putStrLn $ unwords [show server, T.unpack (P.source msg'), "->", "broker"]
                  sockets' <- readMVar sockets
                  modifyMVar_ broker $ onProductMessage opts msg' sockets'
          Z.send regSocket [] (BS.concat $ BSL.toChunks (A.encode (RS.RegisterServiceResponse serviceID "ok" $ Just port)))
        Nothing -> yield

    _ <- readMVar interrupted
    killThread sourceThread
    killThread registerThread
--    forM_ threads killThread

runSourceIO :: Options -> Either WS.Connection (Socket Sub) -> MVar Sockets -> MVar Broker -> IO ThreadId
runSourceIO opts src sockets broker =
  forkIO $ forever $ do
    msg <- A.decodeStrict <$> (either receiveFromWS receiveFromZMQ src)
    for_ msg $ \msg' -> do
      when (debug opts) $ putStrLn $ unwords ["version", T.unpack (V.source msg'),"->", "broker"]
      sockets' <- readMVar sockets
      modifyMVar_ broker $ onVersionMessage opts msg' sockets'

onDeregisterMessage :: Z.Sender a => D.DeregisterService -> MVar Broker -> Socket a -> IO()
onDeregisterMessage deregMsg broker socket = do
  putStrLn $ unwords ["deregister", T.unpack (D.deregisterServiceID deregMsg), "->", "broker"]
  modifyMVar_ broker (B.deregisterService (D.deregisterServiceID deregMsg))
  Z.send socket [] ""

myInsert :: (Ord k) => k -> v -> Map k v -> IO (Map k v)
myInsert k v m = do
  return (M.insert k v m)

receiveFromWS :: WS.Connection -> IO BS.ByteString
receiveFromWS src = do
  wsMsg <- (WS.receiveData src :: IO BSL.ByteString)
  evaluate $ BS.concat $ BSL.toChunks wsMsg

receiveFromZMQ :: Socket Sub -> IO BS.ByteString
receiveFromZMQ src =
  Z.receive src

sendToWS :: BS.ByteString -> WS.Connection -> IO ()
sendToWS rawMsg snk =
  WS.sendTextData snk rawMsg

sendToZMQ :: BS.ByteString -> Socket Pub -> IO ()
sendToZMQ rawMsg snk =
  Z.send snk [] rawMsg

type Sockets = Map Server (Socket Pair)

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