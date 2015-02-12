{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Monto.Broker
  ( empty
  , register
  , registerProductDep
  , Broker
  , newVersion
  , newProduct
  , Response (..)
  , Message (..)
  , dynamicDependencyManagerToDot
  , staticDependencyManagerToDot
  )
  where

import           Control.Applicative hiding (empty)

import           Data.Maybe (fromJust,isJust)
import           Data.Text (Text)

import           Monto.Types (Source)
import           Monto.VersionMessage (VersionMessage)
import           Monto.ProductMessage (ProductMessage)
import           Monto.ResourceManager (ResourceManager)
import qualified Monto.ResourceManager as R
import           Monto.DependencyManager (Dependency(..),Server(..),ProductDependency(..))
import qualified Monto.DependencyManager as Dep
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
  deriving (Eq,Show)
data Response = Response Source Server [Message]
  deriving (Eq,Show)

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

registerProductDep :: ProductDependency -> [ProductDependency] -> Broker -> ([Source],Broker)
registerProductDep from to broker =
  let (srcs,dynamicDepMgr') = D.register from to (resourceMgr broker) (dynamicDepMgr broker)
      broker' = broker { dynamicDepMgr = dynamicDepMgr' }
      srcs'   = flip filter srcs $ \src ->
                  not $ isJust $ R.lookupVersionMessage src (resourceMgr broker)
  in (srcs',broker')

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
      {-res2' = do-}
        {-Dep.Response src server dynamicDependencies <- res2-}
        {-let staticDependencies = S.lookupDependencies src server staticDepMgr' -}
        {-return $ Dep.Response src server (staticDependencies ++ dynamicDependencies)-}
      res = res1 ++ res2
      broker' = broker
        { resourceMgr  = snd $ R.updateProduct' pr $ resourceMgr broker
        , staticDepMgr = staticDepMgr'
        , dynamicDepMgr = dynamicDepMgr'
        }
  in (map (makeResponse broker') res,broker')

makeResponse :: Broker -> Dep.Response -> Response
{-# INLINE makeResponse #-}
makeResponse broker (Dep.Response src server deps) =
  Response src server (map (fromJust . lookupDependency broker) deps)

lookupDependency :: Broker -> ProductDependency -> Maybe Message
lookupDependency broker dep = case dep of
  (Version source)             -> VersionMessage <$> R.lookupVersionMessage source (resourceMgr broker)
  (Product (source,prod,lang)) -> ProductMessage <$> R.lookupProductMessage (source,lang,prod) (resourceMgr broker)

dynamicDependencyManagerToDot :: Broker -> Text
dynamicDependencyManagerToDot = D.automatonToDot . dynamicDepMgr

staticDependencyManagerToDot :: Broker -> Text
staticDependencyManagerToDot = S.automatonToDot . staticDepMgr
