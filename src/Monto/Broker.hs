{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Monto.Broker
  ( empty
  , register
  , Broker
  , newVersion
  , newProduct
  , Response (..)
  , Message (..)
  )
  where

import           Control.Applicative hiding (empty)

import           Data.Maybe (fromJust)

import           Monto.Types
import           Monto.VersionMessage (VersionMessage)
import qualified Monto.VersionMessage as V
import           Monto.ProductMessage (ProductMessage)
import qualified Monto.ProductMessage as P
import           Monto.ServerDependency
import           Monto.ResourceManager (ResourceManager)
import qualified Monto.ResourceManager as R
import           Monto.StaticDependencyManager (StaticDependencyManager)
import qualified Monto.StaticDependencyManager as S

data Broker = Broker
  { resourceMgr  :: ResourceManager
  , staticDepMgr :: StaticDependencyManager
  } deriving (Eq,Show)

data Message = Version VersionMessage | Product ProductMessage
data Response = Response Server [Message]

empty :: Broker
{-# INLINE empty #-}
empty = Broker
  { resourceMgr  = R.empty
  , staticDepMgr = S.empty
  }

register :: Server -> [ServerDependency] -> Broker -> Broker
{-# INLINE register #-}
register server deps broker
  = broker { staticDepMgr = S.register server deps (staticDepMgr broker) }

newVersion :: VersionMessage -> Broker -> ([Response],Broker)
{-# INLINE newVersion #-}
newVersion version broker =
  let (res,staticDepMgr') = S.newVersion version (staticDepMgr broker)
      broker' = broker
        { resourceMgr  = snd $ R.updateVersion version $ resourceMgr broker
        , staticDepMgr = staticDepMgr'
        }
  in (map (makeResponse (V.source version) broker') res,broker')

newProduct :: ProductMessage -> Broker -> ([Response],Broker)
{-# INLINE newProduct #-}
newProduct pr broker =
  let (res,staticDepMgr') = S.newProduct pr (staticDepMgr broker)
      broker' = broker
        { resourceMgr  = snd $ R.updateProduct' pr $ resourceMgr broker
        , staticDepMgr = staticDepMgr'
        }
  in (map (makeResponse (P.source pr) broker') res,broker')

makeResponse :: Source -> Broker -> S.Response -> Response
{-# INLINE makeResponse #-}
makeResponse source broker (S.Response server deps) =
  Response server (map (fromJust . lookupDependency source broker) deps)

lookupDependency :: Source -> Broker -> ServerDependency -> Maybe Message
lookupDependency source broker dep = case dep of
  Source -> Version <$> R.lookupVersionMessage source (resourceMgr broker)
  Server prod lang -> Product <$> R.lookupProductMessage (source,lang,prod) (resourceMgr broker)
  Star -> error "Star cannot be a dependency"
