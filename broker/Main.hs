{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.ZMQ4 hiding (message)
import           System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)

import           Systemd.Journal

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad

import qualified Data.Aeson as A
import           Data.Foldable (for_)
import           Data.Text (Text)
import qualified Data.Text as T

import           Monto.MessageStore (MessageStore)
import qualified Monto.MessageStore as Store
import           Monto.VersionMessage (VersionMessage)
import qualified Monto.VersionMessage as V
import           Monto.ProductMessage (ProductMessage)
import qualified Monto.ProductMessage as P

import           Options.Applicative

data Options = Options
  { debug :: Bool
  }

options :: Parser Options
options = Options <$>
  switch (  long "debug"
         <> help "print messages that are transmitted over the broker"
         )

data Sockets = Sockets
  { fromSources :: Socket Sub
  , toServers   :: Socket Pub
  , fromServers :: Socket Sub
  , toSinks     :: Socket Pub
  }

onVersionMessage :: Options -> VersionMessage -> Sockets -> MessageStore -> IO MessageStore
{-# INLINE onVersionMessage #-}
onVersionMessage opts versionMessage sockets store = do
  let (invalid,store') = Store.updateVersion versionMessage store
      response         = A.encode $ versionMessage { V.invalid = Just invalid }
  when (debug opts) $ sendMessageWith (textShow (Store.versionId versionMessage)) (priority Debug)
  send' (toServers sockets) [] response
  return store'

onProductMessage :: Options -> ProductMessage -> Sockets -> MessageStore -> IO MessageStore
{-# INLINE onProductMessage #-}
onProductMessage opts productMessage sockets store = do
  case Store.updateProduct productMessage store of
    Just (invalid,store') -> do
      let response = A.encode $ productMessage { P.invalid = Just invalid }
      when (debug opts) $ sendMessageWith (textShow (Store.productId productMessage)) (priority Debug)
      send' (toSinks sockets) [] response
      return store'
    Nothing -> do
      sendMessageWith (
        "Some dependencies are not there\n"
        <> "product message: " <> textShow (Store.productId productMessage) <> "\n"
        <> "dependencies: " <> textShow (P.dependencies productMessage) <> "\n"
        <> "store: " <> textShow store)
        (priority Warning)
      return store

data Interrupted = Interrupted
  deriving (Eq,Show)

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> options)
    ( fullDesc
    <> progDesc "Monto Broker"
    )
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

      _ <- installHandler sigINT  (Catch (stopExcecution interrupted)) Nothing
      _ <- installHandler sigTERM (Catch (stopExcecution interrupted)) Nothing

      sourcesToServers <- forkIO $ forever $ do
        msg <- A.decodeStrict <$> receive fromSourcesSocket 
        for_ msg $ \msg' -> modifyMVar_ messageStore $ onVersionMessage opts msg' sockets

      productsToSinks <- forkIO $ forever $ do
        msg <- A.decodeStrict <$> receive fromServersSocket
        for_ msg $ \msg' -> modifyMVar_ messageStore $ onProductMessage opts msg' sockets

      _ <- readMVar interrupted
      killThread sourcesToServers
      killThread productsToSinks

stopExcecution :: MVar Interrupted -> IO ()
stopExcecution interrupted = do
  putMVar interrupted Interrupted

textShow :: Show a => a -> Text
textShow = T.pack . show
