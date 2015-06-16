{-# LANGUAGE CPP #-}
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

import           Prelude hiding (product)

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative hiding (empty)
#endif

import           Control.Monad (guard)

import           Data.Maybe (fromJust,fromMaybe,isJust,catMaybes)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Graph.Inductive (Gr,Node)
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

updateAutomaton :: Ord dep => Gr dep () -> Automaton (Map dep L) dep (Set dep)
{-# INLINE updateAutomaton #-}
updateAutomaton gr =
  foldr1 A.merge $ map (uncurry A.buildAutomaton . outEdges) $ G.bfs 0 $ G.grev gr
  where
    outEdges n =
      let (Just (_,_,a,out),_) = G.match n gr
      in (a,[ fromJust (G.lab gr o) | (_,o) <- out ])

lookupDependencies :: Ord dep => Dependency dep -> DependencyGraph dep -> [Dependency dep]
lookupDependencies = lookupDeps G.suc

lookupReverseDependencies :: Ord dep => Dependency dep -> DependencyGraph dep -> [Dependency dep]
lookupReverseDependencies = lookupDeps G.pre

lookupDeps :: Ord dep => (Gr (Dependency dep) () -> Node -> [Node]) -> Dependency dep -> DependencyGraph dep -> [Dependency dep]
lookupDeps direction dep dg =
  case M.lookup dep (DG.nodeMap dg) of
    Just depNode -> do
      nodes <- direction (DG.dependencies dg) depNode
      return $ fromJust $ G.lab (DG.dependencies dg) nodes
    Nothing -> []

newVersion :: VersionMessage -> Broker -> ([Response],Broker)
{-# INLINE newVersion #-}
newVersion version broker =
  let process = A.start (A.compileAutomaton (automaton broker))
      (res,process') = fromMaybe (S.empty,process) $ A.runProcess Bottom process
      broker' = broker
        { processes   = M.insert (V.source version) process' (processes broker)
        , resourceMgr = snd $ R.updateVersion version $ resourceMgr broker
        }
  in (catMaybes $ map (\(Dependency (Server prod lang)) -> makeResponse broker' (V.source version) prod lang) (S.toList res),broker')

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
        res' = lookupReverseDependencies (Dependency (Product (P.source pr, P.language pr,P.product pr))) (productDependencies broker)
        broker' = broker
          { resourceMgr = snd $ R.updateProduct' pr $ resourceMgr broker
          , processes   = M.insert (P.product pr) process' (processes broker)
          }
        staticResponse  = map (\(Dependency (Server prod lang)) -> makeResponse broker' (P.source pr) prod lang) (S.toList res)
        dynamicResponse = map (\(Dependency (Product (src,lang,prod))) -> makeResponse broker' src prod lang) res'
    in (catMaybes (staticResponse ++ dynamicResponse),broker')

makeResponse :: Broker -> Source -> Product -> Language -> Maybe Response
{-# INLINE makeResponse #-}
makeResponse broker source product language =
  let sdeps = lookupDependencies (Dependency (Server product language)) (serviceDependencies broker)
      pdeps = lookupDependencies (Dependency (Product (source,language,product))) (productDependencies broker)
      deps = map (depForServer source) sdeps ++ map (\(Dependency p) -> p) pdeps
      msgs = catMaybes $ map (lookupDependency broker) deps
  in if all isJust $ map (\(Dependency p) -> lookupDependency broker p) pdeps
      then Just $ Response source (Server product language) msgs
      else Nothing

depForServer :: Source -> ServerDependency -> ProductDependency
depForServer source (Dependency (Server prod lang)) = Product (source,lang,prod)
depForServer source Bottom                          = Version source
depForServer _      Top                             = error "top is no dependency"

lookupDependency :: Broker -> ProductDependency -> Maybe Message
lookupDependency broker dep = case dep of
  (Version source) -> VersionMessage <$> R.lookupVersionMessage source (resourceMgr broker)
  (Product p)      -> ProductMessage <$> R.lookupProductMessage p (resourceMgr broker)
