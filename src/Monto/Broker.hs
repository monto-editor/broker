{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Monto.Broker
  ( empty
  , registerServer
  , registerProductDep
  , Broker
  , newVersion
  , newProduct
  , Response (..)
  , Message (..)
  , Dependency(..)
  , Server(..)
  , ServerDependency
  , ProductDependency(..)
  )
  where

import           Control.Applicative hiding (empty)
import           Control.Monad (guard)

import           Data.Maybe (fromJust,fromMaybe,isJust,catMaybes)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Graph.Inductive (Gr)
import qualified Data.Graph.Inductive as G

import           Monto.Automaton (Automaton(..),L,Process)
import qualified Monto.Automaton as A
import           Monto.Types (Source,Language,Product)
import           Monto.VersionMessage (VersionMessage)
import           Monto.ProductMessage (ProductMessage)
import           Monto.ResourceManager (ResourceManager)
import qualified Monto.ResourceManager as R
import           Monto.DependencyGraph (DependencyGraph,Dependency(..))
import qualified Monto.DependencyGraph as DG
import qualified Monto.VersionMessage as V
import qualified Monto.ProductMessage as P

data ProductDependency
  = Version Source
  | Product (Source,Language,Product)
  deriving (Eq,Ord)

data Print = Print Text
instance Show Print where
  show (Print s) = T.unpack s

instance Show ProductDependency where
  show (Version src) = T.unpack src
  show (Product (src,prod,lang)) = show (Print src,Print prod,Print lang)

data Server = Server Product Language
  deriving (Eq,Ord)

instance Show Server where
  show (Server p l) = concat [ T.unpack p, "/", T.unpack l ]

instance Read Server where
  readsPrec _ r = do
    (a,r') <- lex r
    (b,r'') <- lex r'
    (c,r''') <- lex r''
    guard $ b == "/"
    return (Server (T.pack a) (T.pack c),r''')

type ServerDependency = Dependency Server

data Message = VersionMessage VersionMessage | ProductMessage ProductMessage
  deriving (Eq,Show)

data Response = Response Source Server [Message]
  deriving (Eq,Show)

data Broker = Broker
  { resourceMgr         :: ResourceManager
  , serviceDependencies :: DependencyGraph Server
  , productDependencies :: DependencyGraph ProductDependency
  , automaton           :: Automaton (Map ServerDependency L) ServerDependency (Set ServerDependency)
  , processes           :: Map Source (Process (Map ServerDependency L) ServerDependency (Set ServerDependency))
  } deriving (Eq,Show)

empty :: Broker
{-# INLINE empty #-}
empty = Broker
  { resourceMgr = R.empty
  , serviceDependencies = DG.empty
  , productDependencies = DG.empty
  , automaton = Automaton
      { initialState = M.empty
      , transitions  = []
      , accepting    = []
      }
  , processes = M.empty
  }

registerServer :: Server -> [ServerDependency] -> Broker -> Broker
{-# INLINE registerServer #-}
registerServer server deps broker =
  let serviceDependencies' = DG.register server deps (serviceDependencies broker)
  in broker
  { serviceDependencies = serviceDependencies'
  , automaton = updateAutomaton (DG.dependencies serviceDependencies')
  }

updateAutomaton :: Ord dep => Gr dep () -> Automaton (Map dep L) dep (Set dep)
{-# INLINE updateAutomaton #-}
updateAutomaton gr =
  foldr1 A.merge $ map (uncurry A.buildAutomaton . outEdges) $ G.bfs 0 $ G.grev gr
  where
    outEdges n =
      let (Just (_,_,a,out),_) = G.match n gr
      in (a,[ fromJust (G.lab gr o) | (_,o) <- out ])

lookupDependencies :: Ord dep => Dependency dep -> DependencyGraph dep -> [Dependency dep]
lookupDependencies dep manager =
  let depNode = DG.nodeMap manager M.! dep
  in do
    successor <- G.suc (DG.dependencies manager) depNode
    return $ fromJust $ G.lab (DG.dependencies manager) successor

registerProductDep :: ProductDependency -> [ProductDependency] -> Broker -> ([Source],Broker)
registerProductDep from to broker =
  let productDependencies' = DG.register from (map Dependency to) (productDependencies broker)
      srcs    = flip map to $ \dep -> 
        case dep of
          Version src       -> src
          Product (src,_,_) -> src
      broker' = broker { productDependencies = productDependencies' }
      srcs'   = flip filter srcs $ \src ->
                  not $ isJust $ R.lookupVersionMessage src (resourceMgr broker)
  in (srcs',broker')

newVersion :: VersionMessage -> Broker -> ([Response],Broker)
{-# INLINE newVersion #-}
newVersion version broker =
  let process = A.start (A.compileAutomaton (automaton broker))
      (res,process') = fromMaybe (S.empty,process) $ A.runProcess Bottom process
      broker' = broker
        { processes   = M.insert (V.source version) process' (processes broker)
        , resourceMgr = snd $ R.updateVersion version $ resourceMgr broker
        }
  in (map (makeResponse broker' (V.source version)) (S.toList res),broker')

newProduct :: ProductMessage -> Broker -> ([Response],Broker)
{-# INLINE newProduct #-}
newProduct pr broker
  | R.isOutdated pr (resourceMgr broker) = ([],broker)
  | otherwise =
    let process = fromMaybe (A.start (A.compileAutomaton (automaton broker)))
                $ M.lookup (P.source pr)
                $ processes broker
        (res,process') = fromMaybe (S.empty,process)
                       $ A.runProcess (Dependency (Server (P.product pr) (P.language pr))) process
        broker' = broker
          { resourceMgr = snd $ R.updateProduct' pr $ resourceMgr broker
          , processes   = M.insert (P.product pr) process' (processes broker)
          }
    in (map (makeResponse broker' (P.source pr)) (S.toList res),broker')

makeResponse :: Broker -> Source -> ServerDependency -> Response
{-# INLINE makeResponse #-}
makeResponse broker source s@(Dependency server) =
  let deps = lookupDependencies s (serviceDependencies broker)
      msgs = catMaybes $ map (lookupDependency broker . depForServer source) deps
  in Response source server msgs
makeResponse _ _ _ = error "Cannot create response"

depForServer :: Source -> ServerDependency -> ProductDependency
depForServer source (Dependency (Server prod lang)) = Product (source,prod,lang)
depForServer source Bottom                          = Version source
depForServer _      Top                             = error "top is no dependency"

lookupDependency :: Broker -> ProductDependency -> Maybe Message
lookupDependency broker dep = case dep of
  (Version source)             -> VersionMessage <$> R.lookupVersionMessage source (resourceMgr broker)
  (Product (source,prod,lang)) -> ProductMessage <$> R.lookupProductMessage (source,lang,prod) (resourceMgr broker)
