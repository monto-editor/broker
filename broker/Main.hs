{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           System.Posix.Signals          (Handler (Catch), installHandler,
                                                sigINT, sigTERM)
import           System.ZMQ4                   (Context, Pair, Socket)
import qualified System.ZMQ4                   as Z hiding (message, source)

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import qualified Data.Aeson                    as A
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.List                     as List
import           Data.Map                      (Map)
import qualified Data.Map                      as M
import           Data.Monoid                   ((<>))
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

import           Monto.Broker                  (Broker)
import qualified Monto.Broker                  as B
import           Monto.CommandMessage          (CommandMessage)
import qualified Monto.CommandMessage          as CM
import           Monto.ConfigurationMessage    (ConfigurationMessage (..))
import qualified Monto.DeregisterService       as D
import           Monto.DiscoverResponse        (DiscoverResponse)
import qualified Monto.DiscoverResponse        as DiscoverResp
import qualified Monto.MessagesIDE             as IDE
import qualified Monto.MessagesService         as Service
import qualified Monto.ProductMessage          as P
import qualified Monto.RegisterServiceRequest  as RQ
import qualified Monto.RegisterServiceResponse as RS
import           Monto.Request                 (Request)
import qualified Monto.Request                 as Req
import qualified Monto.Service                 as S
import qualified Monto.SourceMessage           as S
import           Monto.Types

import           Options.Applicative

import           Text.Printf

type Addr = String
type SocketPool = Map Port (Socket Pair)
type AppState = (Broker, SocketPool)

data Options = Options
  { debug        :: Bool
  , debugGraphs  :: Bool
  , sink         :: Addr
  , source       :: Addr
  , registration :: Addr
  , fromPort     :: Port
  , toPort       :: Port
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
      let stopExecution = putMVar interrupted Interrupted
      _ <- installHandler sigINT  (Catch stopExecution) Nothing
      _ <- installHandler sigTERM (Catch stopExecution) Nothing

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
          when (debug opts) $ printf "%s -> broker\n" (show (S.source msg))
          modifyMVar_ appstate $ onSourceMessage msg
        Right (IDE.ConfigurationMessages msgs) -> do
          when (debug opts) $ printf "config messages -> broker\n"
          --when (debug opts) $ printf "config messages: %s\n" (show msgs)
          withMVar appstate $ \state ->
            forM_ msgs $ \c@(ConfigurationMessage sid _) ->
              sendToService sid (A.encode (Service.ConfigurationMessage c)) state
        Right (IDE.DiscoverRequest request) -> do
          when (debug opts) $ printf "discover request: %s\n" (show request)
          services <- findServices <$> getBroker appstate
          --when (debug opts) $ printf "discover response: %s\n" (show services)
          when (debug opts) $ printf "sending discover response\n"
          Z.send snk [] $ convertBslToBs $ A.encode (IDE.DiscoverResponse services)
        Right (IDE.CommandMessage cmdMsg) -> do
          when (debug opts) $ printf "cmdMsg -> broker: %s\n" (show cmdMsg)
          withMVar appstate $ \state ->
            sendToService (CM.serviceID cmdMsg) (A.encode (Service.CommandMessage cmdMsg)) state

        Left err -> printf "Couldn't parse this message from IDE: %s\nBecause %s\n" (show rawMsg) err
  where
    findServices :: Broker -> [DiscoverResponse]
    findServices b = do
      (B.Service sid label description products _ configuration) <- M.elems $ B.services b
      return $ DiscoverResp.DiscoverResponse sid label description products configuration

    onSourceMessage = onMessage opts B.newVersion


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
          when (debug opts) $ print msg
          modifyMVar_ appstate $ onRegisterDynamicDependenciesMessage msg
        Right (Service.CommandMessageDependency msg) -> do
          when (debug opts) $ print msg
          modifyMVar_ appstate $ onRegisterCommandMessageDependenciesMessage msg
        Left err -> printf "Couldn't parse this message from service: %s\nBecause %s\n" (show rawMsg) err
  where
    onProductMessage = onMessage opts B.newProduct
    onRegisterDynamicDependenciesMessage = onMessage opts (\msg broker -> let (maybeRequest, broker') = B.newDynamicDependency msg broker
                                                                          in  ((maybeRequest,Nothing),broker'))
    onRegisterCommandMessageDependenciesMessage  = onMessage opts (\msg broker -> let (maybeCommandMessage, broker') = B.newCommandMessageDependency msg broker
                                                                                  in  ((Nothing,maybeCommandMessage),broker'))

maybeT :: Monad m => Maybe a -> MaybeT m a
maybeT = MaybeT . return

getBroker :: MVar AppState -> IO Broker
getBroker = fmap fst . readMVar

sendToService :: ServiceID -> BSL.ByteString -> AppState -> IO ()
sendToService sid msg (broker,pool) = void $ runMaybeT $ do
  port <- maybeT $ S.port <$> M.lookup sid (B.services broker)
  socket <- maybeT $ M.lookup port pool
  lift $ Z.send socket [] $ convertBslToBs msg

onMessage :: Foldable f => Options -> (message -> Broker -> ((f Request,f CommandMessage),Broker)) -> message -> AppState -> IO AppState
{-# INLINE onMessage #-}
onMessage opts handler msg (broker,pool) = do
  let ((requests,cmdMgs),broker') = handler msg broker
  forM_ requests $ \request -> do
    when (debug opts) $ printf "broker -> %s\n" (toText (Req.serviceID request))
    sendToService (Req.serviceID request) (A.encode (Service.Request request)) (broker',pool)
  forM_ cmdMgs $ \cmdMsg -> do
    when (debug opts) $ printf "broker -> %s (cmdMsg: %s)\n" (toText (CM.serviceID cmdMsg)) (show cmdMsg)
    sendToService (CM.serviceID cmdMsg) (A.encode (Service.CommandMessage cmdMsg)) (broker',pool)
  return (broker', pool)

convertBslToBs :: BSL.ByteString -> BS.ByteString
convertBslToBs msg =
  BS.concat $ BSL.toChunks msg

data Interrupted = Interrupted
  deriving (Eq,Show)
