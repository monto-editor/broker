{-# LANGUAGE OverloadedStrings,ScopedTypeVariables #-}
module Main where

import           System.ZMQ4 (Pair,Context,Pub,Sub,Socket)
import qualified System.ZMQ4 as Z hiding (message,source)
--import           System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)

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
--import qualified Data.Text.Encoding as TE

import           Monto.Broker (Broker,Response,Server,ServerDependency,Service)
import qualified Monto.Broker as B
import           Monto.VersionMessage (VersionMessage)
import           Monto.ProductMessage (ProductMessage)
import qualified Monto.VersionMessage as V
import qualified Monto.ProductMessage as P
import qualified Monto.RegisterServiceRequest as RQ
import qualified Monto.RegisterServiceResponse as RS
import qualified Monto.DeregisterService as D
import           Monto.Types (ServiceID)

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
  , servers       :: [(Server,[ServerDependency],Addr,ServiceID)]
  }

options :: Parser Options
options = Options
  <$> switch      (long "debug"        <> help "print messages that are transmitted over the broker")
  <*> switch      (long "websocket"    <> help "use websockets instead of zeromq")
  <*> strOption   (long "sink"         <> help "address of the sink")
  <*> strOption   (long "source"       <> help "address of the source")
  <*> strOption   (long "registration" <> help "address for service registration")
  <*> option auto (long "servers"      <> help "names, their dependencies and their ports" <> metavar "[(Server,[ServerDependency],Address,ServiceID)]")

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> options)
    ( fullDesc
    <> progDesc "Monto Broker"
    )
  runServer opts

runServer :: Options -> IO ()
runServer opts = do
  Z.withContext $ \ctx ->

    Z.withSocket ctx Z.Rep $ \registerSckt -> do
      brkr <- newMVar B.empty

      Z.bind registerSckt (registration opts) `catch` \(e :: SomeException) -> do
        putStrLn $ unwords ["couldn't bind address", (registration opts), "for registrations"]
        throw e

      putStrLn $ unwords ["listen on address", (registration opts), "for registrations"]
      registerThread <- forkIO $ forever $ do
        rawMsg <- Z.receive registerSckt
        let d = (A.decodeStrict rawMsg) :: Maybe D.DeregisterService
        let r = (A.decodeStrict rawMsg) :: Maybe RQ.RegisterServiceRequest
        case d of
          Just _ -> do
            putStrLn $ unwords ["deregister", T.unpack (D.deregisterServiceID $ fromJust d), "->", "broker"]
            modifyMVar_ brkr (B.deregisterService (D.deregisterServiceID $ fromJust d))
            Z.send registerSckt [] ""
          Nothing -> yield
        case r of
          Just _ -> do
            putStrLn $ unwords ["register", T.unpack (RQ.registerServiceID $ fromJust r), "->", "broker"]
            modifyMVar_ brkr (B.registerService (B.Server (RQ.product $ fromJust r) (RQ.language $ fromJust r)) (RQ.registerServiceID $ fromJust r))
--            modifyMVar_ brkr (B.registerServer (B.Server (RQ.product $ fromJust r))
            b <- readMVar brkr
            let service = head (List.filter (\(B.Service _ serviceID _) -> serviceID == (RQ.registerServiceID $ fromJust r)) (B.servers b))
            Z.send registerSckt [] (BS.concat $ BSL.toChunks (A.encode (RS.RegisterServiceResponse (RQ.registerServiceID $ fromJust r) "ok" $ Just (B.port service))))
          Nothing -> yield

      withServers ctx B.empty (servers opts) $ \b0 sockets -> do
        broker <- newMVar b0
        interrupted <- newEmptyMVar

        if websocket opts
          then
            WS.runServer "127.0.0.1" (read $ source opts :: Int) $ \srcSock -> do
              src <- WS.acceptRequest srcSock
              putStrLn srcMsg

              WS.runServer "127.0.0.1" (read $ sink opts :: Int) $ \snkSock -> do
                snk <- WS.acceptRequest snkSock
                putStrLn snkMsg
                handleIO (Left src) (Left snk) sockets opts broker interrupted registerThread
          else
            Z.withSocket ctx Z.Sub $ \src -> do
              Z.bind src $ source opts
              Z.subscribe src ""
              putStrLn srcMsg

              Z.withSocket ctx Z.Pub $ \snk -> do
                Z.bind snk $ sink opts
                putStrLn snkMsg
                handleIO (Right src) (Right snk) sockets opts broker interrupted registerThread
        where
          srcMsg = unwords ["listen on address", show $ source opts, "for versions"]
          snkMsg = unwords ["publish all products to sink on address", show $ sink opts]

handleIO :: MontoSrc -> MontoSnk -> Sockets -> Options -> MVar Broker -> MVar t -> ThreadId -> IO ()
handleIO src snk sockets opts broker interrupted registerThread = do
  sourceThread <- forkIO $ forever $ do
    msg <- A.decodeStrict <$> (either receiveFromWS receiveFromZMQ src)
    for_ msg $ \msg' -> do
      when (debug opts) $ putStrLn $ unwords ["version", T.unpack (V.source msg'),"->", "broker"]
      modifyMVar_ broker $ onVersionMessage opts msg' sockets
  threads <- forM (M.toList sockets) $ \(server,sckt) ->
    forkIO $ forever $ do
      rawMsg <- Z.receive sckt
      either (sendToWS rawMsg) (sendToZMQ rawMsg) snk
      let msg = A.decodeStrict rawMsg
      for_ msg $ \msg' -> do
        when (debug opts) $ putStrLn $ unwords [show server, T.unpack (P.source msg'), "->", "broker"]
        modifyMVar_ broker $ onProductMessage opts msg' sockets
  _ <- readMVar interrupted
  killThread sourceThread
  killThread registerThread
  forM_ threads killThread

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

withServers :: Context -> Broker -> [(Server,[ServerDependency],Addr,ServiceID)] -> (Broker -> Sockets -> IO b) -> IO b
withServers ctx b0 s k = go b0 s M.empty
  where
    go b ((server,deps,addr,_):rest) sockets = do
      Z.withSocket ctx Z.Pair $ \sckt -> do
        Z.bind sckt addr `catch` \(e :: SomeException) -> do
          putStrLn $ unwords ["couldn't bind address", addr, "for server", show server]
          throw e
        putStrLn $ unwords ["listen on address", addr, "for", show server]
        go (B.registerServer server deps b) rest (M.insert server sckt sockets)
    go b [] sockets = k b sockets

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