{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.ZMQ4

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad

import qualified Data.Aeson as A
import           Data.Foldable (for_)
import           Data.Map (Map)
import qualified Data.Map as M
import           Monto.VersionMessage (VersionMessage,Source,Language)
import qualified Monto.VersionMessage as V
import           Monto.ProductMessage (ProductMessage,Product)
import qualified Monto.ProductMessage as P

data Dependency = Dependency Source Language Product

data Sockets = Sockets
  { fromSources :: Socket XSub
  , toServers   :: Socket XPub
  , fromServers :: Socket XSub
  , toSinks     :: Socket XPub
  }

type ProductMap = Map (Source,Language,Product) ProductMessage
type DependencyMap = Map Int Dependency

data MessageStore = MessageStore
  { versionMessages :: Map (Source,Language) VersionMessage
  , productMessages :: ProductMap
  , dependencies    :: DependencyMap
  , sockets         :: Sockets
  }

emptyMessageStore = MessageStore M.empty M.empty M.empty

onVersionMessage :: VersionMessage -> MessageStore -> IO MessageStore
onVersionMessage newVersion (MessageStore versionMessages productMessages dependencies sockets) = do
  let z = M.insert (V.source newVersion,V.language newVersion) versionMessages
      z' = invalidateProductMessages newVersion productMessages dependencies
  undefined

invalidateProductMessages :: VersionMessage -> ProductMap -> DependencyMap -> ProductMap
invalidateProductMessages = undefined

onProductMessage :: ProductMessage -> MessageStore -> IO MessageStore
onProductMessage = undefined

main = do
  withContext $ \ctx ->
    withSocket ctx XSub $ \fromSources ->
    withSocket ctx XPub $ \toServers ->
    withSocket ctx XSub $ \fromServers ->
    withSocket ctx XPub $ \toSinks -> do
      bind fromSources "tcp://127.0.0.1:5000"
      bind toServers   "tcp://127.0.0.1:5001"
      bind fromServers "tcp://127.0.0.1:5002"
      bind toSinks     "tcp://127.0.0.1:5003"

      messageStore <- newMVar $ emptyMessageStore $
        Sockets fromSources toServers fromServers toSinks

      forkIO $ forever $ do
        msg <- A.decodeStrict <$> receive fromSources
        for_ msg $ \msg' -> withMVar messageStore $ onVersionMessage msg'

      forkIO $ forever $ do
        msg <- A.decodeStrict <$> receive fromServers
        for_ msg $ \msg' -> withMVar messageStore $ onProductMessage msg'
