module Main where

import           System.ZMQ4 hiding (message)
import           System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad

import qualified Data.Aeson as A
import           Data.Foldable (for_)
import           Data.Map (Map)
import qualified Data.Map as M

import           Monto.ServerDependency (Server,ServerDependency(..))
import           Monto.Broker (Broker,Response)
import qualified Monto.Broker as B
import           Monto.VersionMessage (VersionMessage)
import           Monto.ProductMessage (ProductMessage)
{-import qualified Monto.VersionMessage as V-}
{-import qualified Monto.ProductMessage as P-}

import           Options.Applicative

type Addr = String

data Options = Options
  { debug   :: Bool
  , servers :: [(Server,[ServerDependency],Addr)]
  }

options :: Parser Options
options = Options
  <$> switch      (long "debug"   <> help "print messages that are transmitted over the broker")
  <*> option auto (long "servers" <> help "names, their dependencies and their ports" )

start :: IO ()
start = do
  opts <- execParser $ info (helper <*> options)
    ( fullDesc
    <> progDesc "Monto Broker"
    )
  withContext $ \ctx ->
    withServers ctx B.empty (servers opts) $ \b0 sockets -> do

      broker <- newMVar b0
      interrupted <- newEmptyMVar

      let stopExcecution = putMVar interrupted Interrupted
      _ <- installHandler sigINT  (Catch stopExcecution) Nothing
      _ <- installHandler sigTERM (Catch stopExcecution) Nothing

      threads <- forM (M.toList sockets) $ \(server,sckt) ->
        case server of
          Source -> forkIO $ forever $ do
            msg <- A.decodeStrict <$> receive sckt
            for_ msg $ \msg' -> modifyMVar_ broker $ onVersionMessage opts msg' sockets
          _ -> forkIO $ forever $ do
            msg <- A.decodeStrict <$> receive sckt
            for_ msg $ \msg' -> modifyMVar_ broker $ onProductMessage opts msg' sockets

      _ <- readMVar interrupted
      forM_ threads killThread

type Sockets = Map Server (Socket Pair)

withServers :: Context -> Broker -> [(Server,[ServerDependency],Addr)] -> (Broker -> Sockets -> IO b) -> IO b
withServers ctx b0 s k = go b0 s M.empty
  where
    go b ((server,deps,addr):rest) sockets = do
      withSocket ctx Pair $ \sckt -> do
        bind sckt addr
        go (B.register server deps b) rest (M.insert server sckt sockets)
    go b [] sockets = k b sockets

main :: IO ()
main = start

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
sendResponse opts sockets (B.Response server reqs) = do
  let response = A.encode $ A.toJSON $ map toJSON reqs
  send' (sockets M.! server) [] response
  when (debug opts) $ putStrLn $ "Response -> " ++ show server
  where
    toJSON req = case req of
      B.Version vers -> A.toJSON vers
      B.Product prod -> A.toJSON prod

data Interrupted = Interrupted
  deriving (Eq,Show)
