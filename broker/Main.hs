{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.ZMQ4
import           System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad

import qualified Data.Aeson as A
import           Data.Foldable (for_)

import           Monto.MessageStore (MessageStore)
import qualified Monto.MessageStore as Store
import           Monto.VersionMessage (VersionMessage)
import qualified Monto.VersionMessage as V
import           Monto.ProductMessage (ProductMessage)
import qualified Monto.ProductMessage as P

data Sockets = Sockets
  { fromSources :: Socket Sub
  , toServers   :: Socket Pub
  , fromServers :: Socket Sub
  , toSinks     :: Socket Pub
  }

onVersionMessage :: VersionMessage -> Sockets -> MessageStore -> IO MessageStore
onVersionMessage versionMessage sockets store = do
  let (invalid,store') = Store.updateVersion versionMessage store
      response         = A.encode $ versionMessage { V.invalid = Just invalid }
  send' (toServers sockets) [] response
  return store'
{-# INLINE onVersionMessage #-}

onProductMessage :: ProductMessage -> Sockets -> MessageStore -> IO MessageStore
onProductMessage productMessage sockets store = do
  case Store.updateProduct productMessage store of
    Just (invalid,store') -> do
      let response = A.encode $ productMessage { P.invalid = Just invalid }
      send' (toSinks sockets) [] response
      return store'
    Nothing ->
      return store
{-# INLINE onProductMessage #-}

data Interrupted = Interrupted
  deriving (Eq,Show)

main :: IO ()
main = do
  withContext $ \ctx ->
    withSocket ctx Sub $ \fromSourcesSocket ->
    withSocket ctx Pub $ \toServersSocket ->
    withSocket ctx Sub $ \fromServersSocket ->
    withSocket ctx Pub $ \toSinksSocket -> do
      let sockets = Sockets fromSourcesSocket toServersSocket fromServersSocket toSinksSocket

      bind fromSourcesSocket "tcp://*:5000"
      bind toServersSocket   "tcp://*:5001"
      bind fromServersSocket "tcp://*:5002"
      bind toSinksSocket     "tcp://*:5003"

      subscribe fromSourcesSocket ""
      subscribe fromServersSocket ""

      messageStore <- newMVar $ Store.empty
      interrupted <- newEmptyMVar

      _ <- installHandler sigINT (Catch $ stopExcecution interrupted) Nothing
      _ <- installHandler sigTERM (Catch $ stopExcecution interrupted) Nothing

      sourcesToServers <- forkIO $ forever $ do
        msg <- A.decodeStrict <$> receive fromSourcesSocket 
        for_ msg $ \msg' -> modifyMVar_ messageStore $ onVersionMessage msg' sockets

      productsToSinks <- forkIO $ forever $ do
        msg <- A.decodeStrict <$> receive fromServersSocket
        for_ msg $ \msg' -> modifyMVar_ messageStore $ onProductMessage msg' sockets

      putStrLn "running"
      _ <- readMVar interrupted
      killThread sourcesToServers
      killThread productsToSinks
      putStrLn "bye"

stopExcecution :: MVar Interrupted -> IO ()
stopExcecution interrupted = do
  putStrLn "shutdown"
  putMVar interrupted Interrupted
