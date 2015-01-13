{-# LANGUAGE OverloadedStrings #-}
module Monto.ServerDependency where

import           Prelude hiding (product)

import           Control.Monad (guard)

import qualified Data.Text as T

import           Monto.Types

data ServerDependency = Server { product :: Product, language :: Language } | Source | Star
  deriving (Eq,Ord)
type Server = ServerDependency

instance Show ServerDependency where
  show (Server p l) = T.unpack $ T.concat [p, "/", l]
  show Source = "Source"
  show Star = "*"

instance Read ServerDependency where
  readsPrec _ r = do
    (a,r') <- lex r
    if a == "Source"
      then return (Source,r')
      else do
        (b,r'') <- lex r'
        (c,r''') <- lex r''
        guard $ b == "/"
        return (Server (T.pack a) (T.pack c),r''')
