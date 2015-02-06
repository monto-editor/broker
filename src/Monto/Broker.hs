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

import           Monto.VersionMessage (VersionMessage)
import           Monto.ProductMessage (ProductMessage)
import           Monto.ResourceManager (ResourceManager)
import qualified Monto.ResourceManager as R
import           Monto.DependencyManager (Dependency(..),Server(..),ProductDependency(..))
import           Monto.StaticDependencyManager (StaticDependencyManager)
import qualified Monto.StaticDependencyManager as S
import           Monto.DynamicDependencyManager (DynamicDependencyManager)
import qualified Monto.DynamicDependencyManager as D

data Broker = Broker
  { resourceMgr   :: ResourceManager
  , staticDepMgr  :: StaticDependencyManager
  , dynamicDepMgr :: DynamicDependencyManager
  } deriving (Eq,Show)

data Message = VersionMessage VersionMessage | ProductMessage ProductMessage
data Response = Response Server [Message]

empty :: Broker
{-# INLINE empty #-}
empty = Broker
  { resourceMgr   = R.empty
  , staticDepMgr  = S.empty
  , dynamicDepMgr = D.empty
  }

register :: Server -> [Dependency Server] -> Broker -> Broker
{-# INLINE register #-}
register server deps broker = broker
  { staticDepMgr = S.register server deps (staticDepMgr broker)
  }

newVersion :: VersionMessage -> Broker -> ([Response],Broker)
{-# INLINE newVersion #-}
newVersion version broker =
  let (res1,staticDepMgr') = S.newVersion version (staticDepMgr broker)
      (res2,dynamicDepMgr') = D.newVersion version (dynamicDepMgr broker)
      res = res1 ++ res2
      broker' = broker
        { resourceMgr   = snd $ R.updateVersion version $ resourceMgr broker
        , staticDepMgr  = staticDepMgr'
        , dynamicDepMgr = dynamicDepMgr'
        }
  in (map (makeResponse broker') res,broker')

newProduct :: ProductMessage -> Broker -> ([Response],Broker)
{-# INLINE newProduct #-}
newProduct pr broker =
  let (res1,staticDepMgr') = S.newProduct pr (staticDepMgr broker)
      (res2,dynamicDepMgr') = D.newProduct pr (dynamicDepMgr broker)
      res = res1 ++ res2
      broker' = broker
        { resourceMgr  = snd $ R.updateProduct' pr $ resourceMgr broker
        , staticDepMgr = staticDepMgr'
        , dynamicDepMgr = dynamicDepMgr'
        }
  in (map (makeResponse broker') res,broker')

makeResponse :: Broker -> S.Response -> Response
{-# INLINE makeResponse #-}
makeResponse broker (S.Response server deps) =
  Response server (map (fromJust . lookupDependency broker) deps)

lookupDependency :: Broker -> ProductDependency -> Maybe Message
lookupDependency broker dep = case dep of
  (Version source)             -> VersionMessage <$> R.lookupVersionMessage source (resourceMgr broker)
  (Product (source,prod,lang)) -> ProductMessage <$> R.lookupProductMessage (source,lang,prod) (resourceMgr broker)
