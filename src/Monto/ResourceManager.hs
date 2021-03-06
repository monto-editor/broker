{-# LANGUAGE CPP #-}
module Monto.ResourceManager
  ( ResourceManager(..)
  , empty
  , isOutdated
  , updateSource
  , updateProduct
  , lookupSourceMessage
  , lookupProductMessage
  , printResourceManager
  )
  where

import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe

import           Monto.ProductMessage (ProductMessage)
import qualified Monto.ProductMessage as P
import           Monto.Source
import           Monto.SourceMessage  (SourceMessage)
import qualified Monto.SourceMessage  as S
import           Monto.Types

data ResourceManager = ResourceManager
  { sources  :: Map Source SourceMessage
  , products :: Map (Source,ServiceID,Product,Language) ProductMessage
  } deriving (Show,Eq)

empty :: ResourceManager
{-# INLINE empty #-}
empty = ResourceManager M.empty M.empty

isOutdated :: ProductMessage -> ResourceManager -> Bool
isOutdated pr resourceMgr = fromMaybe True $ do
  v <- M.lookup (P.source pr) (sources resourceMgr)
  return $ P.id pr < S.id v

updateSource :: SourceMessage -> ResourceManager -> ResourceManager
{-# INLINE updateSource #-}
updateSource srcMsg resourceMgr@ResourceManager {sources = oldSourceMessages, products = oldProducts} =
  resourceMgr { sources = M.insert src srcMsg oldSourceMessages
              , products = M.filterWithKey (\(s,_,_,_) _ -> s /= src) oldProducts
              }
  where src = S.source srcMsg

updateProduct :: ProductMessage -> ResourceManager -> ResourceManager
{-# INLINE updateProduct #-}
updateProduct prod resourceMgr@ResourceManager {products = oldProducts} =
  resourceMgr { products = M.insert k prod oldProducts }
  where
    k = (P.source prod,P.serviceID prod,P.product prod,P.language prod)

lookupSourceMessage :: Source -> ResourceManager -> Maybe SourceMessage
lookupSourceMessage src resourceMgr = M.lookup src (sources resourceMgr)

lookupProductMessage :: (Source,ServiceID,Product,Language) -> ResourceManager -> Maybe ProductMessage
lookupProductMessage k resourceMgr = M.lookup k (products resourceMgr)

printResourceManager :: ResourceManager -> IO ()
printResourceManager resMgr = do
  putStrLn "ResourceManager"
  putStrLn "- sources:"
  print (sources resMgr)
  putStrLn "- products:"
  print (products resMgr)
