{-# LANGUAGE OverloadedStrings,ScopedTypeVariables #-}
module Main where

import           System.ZMQ4 hiding (message,source)
import           System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)

--import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Exception (throw,catch,SomeException)

import qualified Data.Aeson as A
import           Data.Foldable (for_)
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T

import           Monto.Broker (Broker,Response,Server,ServerDependency)
import qualified Monto.Broker as B
import           Monto.VersionMessage (VersionMessage)
import           Monto.ProductMessage (ProductMessage)
import qualified Monto.VersionMessage as V
import qualified Monto.ProductMessage as P

import           Options.Applicative

type Addr = String

data Options = Options
  { debug   :: Bool
  , sink    :: Addr
  , source  :: Addr
  , servers :: [(Server,[ServerDependency],Addr)]
  }

options :: Parser Options
options = Options
  <$> switch      (long "debug"   <> help "print messages that are transmitted over the broker")
  <*> strOption   (long "sink"    <> help "address of the sink")
  <*> strOption   (long "source"  <> help "address of the source")
  <*> option auto (long "servers" <> help "names, their dependencies and their ports" <> metavar "[(Server,[ServerDependency],Address)]")

start :: IO ()
start = do
  opts <- execParser $ info (helper <*> options)
    ( fullDesc
    <> progDesc "Monto Broker"
    )
  withContext $ \ctx ->

    withSocket ctx Sub $ \src -> do
      bind src (source opts)
      subscribe src ""
      putStrLn $ unwords ["listen on address", source opts, "for versions"]

      withSocket ctx Pub $ \snk -> do
        bind snk (sink opts)
        putStrLn $ unwords ["publish all products to sink on address", sink opts]


        withServers ctx B.empty (servers opts) $ \b0 sockets -> do

          broker <- newMVar b0
          interrupted <- newEmptyMVar

          let stopExcecution = putMVar interrupted Interrupted
          _ <- installHandler sigINT  (Catch stopExcecution) Nothing
          _ <- installHandler sigTERM (Catch stopExcecution) Nothing

          sourceThread <- forkIO $ forever $ do
            msg <- A.decodeStrict <$> receive src
            for_ msg $ \msg' -> do
              when (debug opts) $ putStrLn $ unwords ["version", T.unpack (V.source msg'),"->", "broker"]
              modifyMVar_ broker $ onVersionMessage opts msg' sockets
          threads <- forM (M.toList sockets) $ \(server,sckt) ->
            forkIO $ forever $ do
              rawMsg <- receive sckt
              send snk [] rawMsg
              let msg = A.decodeStrict rawMsg
              for_ msg $ \msg' -> do
                when (debug opts) $ putStrLn $ unwords [show server, T.unpack (P.source msg'), "->", "broker"]
                modifyMVar_ broker $ onProductMessage opts msg' sockets
          _ <- readMVar interrupted
          killThread sourceThread
          forM_ threads killThread

type Sockets = Map Server (Socket Pair)

withServers :: Context -> Broker -> [(Server,[ServerDependency],Addr)] -> (Broker -> Sockets -> IO b) -> IO b
withServers ctx b0 s k = go b0 s M.empty
  where
    go b ((server,deps,addr):rest) sockets = do
      withSocket ctx Pair $ \sckt -> do
        bind sckt addr `catch` \(e :: SomeException) -> do
          putStrLn $ unwords ["couldn't bind address", addr, "for server", show server]
          throw e
        putStrLn $ unwords ["listen on address", addr, "for", show server]
        go (B.registerServer server deps b) rest (M.insert server sckt sockets)
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
sendResponse opts sockets (B.Response src server reqs) = do
  let response = A.encode $ A.toJSON $ map toJSON reqs
  send' (sockets M.! server) [] response
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
