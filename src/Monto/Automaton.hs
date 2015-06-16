{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Monto.Automaton where


#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.Monad hiding (when)

import           Data.Semigroup
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.List (nub)
import           Data.Text (Text)
import qualified Data.Text as T

data Automaton state input output = Automaton
  { initialState :: state
  , transitions  :: [(state,input,state,output)]
  , accepting    :: [state]
  } deriving (Eq,Show)

data L = Bottom | Top
  deriving (Eq,Ord)

instance Show L where
  show Bottom = "⊥"
  show Top = "⊤"

buildAutomaton :: (Ord p) => p -> [p]-> Automaton (Map p L) p (Set p)
buildAutomaton pr requirements = Automaton
  { initialState = initial
  , transitions = requirementsTransitions `mplus` endproductTransitions
  , accepting = [finalState]
  }
  where
    products = requirements ++ [pr]
    initial = M.fromList [ (r,Bottom) | r <- products ]
    requirementsAre l = and . M.elems . M.mapWithKey (\r a -> r `elem` requirements ==> a == l)
    endproductIs l    = and . M.elems . M.mapWithKey (\p a -> p == pr ==> a == l)
    finalState = const Top <$> initial

    requirementsTransitions = do
      i <- requirements
      s <- allStates
      guard (M.lookup i s /= Just Top && endproductIs Bottom s)
      let s' = M.insert i Top s
          o  = if requirementsAre Top s' then S.singleton pr else S.empty
      return (s,i,s',o)

    endproductTransitions = do
      let i = pr
      s <- allStates
      guard (M.lookup i s /= Just Top && requirementsAre Top s)
      let s' = M.insert i Top s
      return (s,i,s',S.empty)

    allStates = M.fromList <$> permutations products [Bottom,Top] 

(==>) :: Bool -> Bool -> Bool
a ==> b = b || not a
infixr 3 ==>

leastUpperBound :: L -> L -> L
leastUpperBound Bottom Bottom = Bottom
leastUpperBound _ _           = Top

instance (Ord i, Eq o, Semigroup o) => Semigroup (Automaton (Map i L) i o) where
  (<>) = merge

merge :: (Ord i, Eq o, Semigroup o) => Automaton (Map i L) i o -> Automaton (Map i L) i o -> Automaton (Map i L) i o
merge a1 a2 = Automaton
  { initialState = initial
  , transitions = elimOrphans $ elimDoublicates $ do
      (s1,i1,s1',o1) <- transitions a1
      (s2,i2,s2',o2) <- transitions a2
      msum
        [ when (s1 === s2 && s1' === s2') $
            let s    = M.union s1 s2
                s1'' = M.insert i1 Top s
                s2'' = M.insert i2 Top s
            in [(s,i1,s1'',o1),(s,i2,s2'',o2)]
        , when (s1' === s2) $
            let s   = M.insert i1 Bottom s'
                s'  = M.union s1' s2
                s'' = M.insert i2 Top s'
            in [(s,i1,s',o1),(s',i2,s'',o2)]
        , when (s2' === s1) $
            let s   = M.insert i2 Bottom s'
                s'  = M.union s2' s1
                s'' = M.insert i1 Top s'
            in [(s,i2,s',o2),(s',i1,s'',o1)]
        ]
  , accepting = [finalState]
  }
  where
    initial = M.union (initialState a1) (initialState a2)
    finalState = fmap (const Top) initial
    s1 === s2 = and (M.elems (M.intersectionWith (==) s1 s2))
    elimDoublicates = map (\((s,i,s'),o) -> (s,i,s',o))
                    . M.toList
                    . M.fromListWith (<>)
                    . map (\(s,i,s',o) -> ((s,i,s'),o))
    elimOrphans trans =
      let alive = checkAlive trans initial S.empty `S.intersection`
                  checkAlive (reverseTrans trans) finalState S.empty
      in filter (\(s,_,s',_) -> s `S.member` alive && s' `S.member` alive) trans

    checkAlive trans state alive =
      let alive' = S.insert state alive
      in foldr (checkAlive trans) alive' $ do
          (s,_,s',_) <- trans
          guard $ s == state && s' `S.notMember` alive
          return s'

    reverseTrans = map (\(s,i,s',o) -> (s',i,s,o))
      
    when b l
      | b         = l
      | otherwise = []

permutations :: [a] -> [b] -> [[(a,b)]]
permutations [] _ = [[]]
permutations (a:as) bs = do
  b <- bs
  rs <- permutations as bs
  return $ (a,b) : rs

states :: Ord s => Automaton s i o -> [s]
states auto = nub
   $ [initialState auto]
  ++ [ s | (s,_,_,_) <- transitions auto ]
  ++ [ s | (_,_,s,_) <- transitions auto ]
  ++ accepting auto

data CompiledAutomaton state input output = CompiledAutomaton
  { initState  :: state
  , transition :: Map (state,input) (state,output)
  , final      :: Set state
  } deriving (Eq,Show)

compileAutomaton :: (Ord s,Ord i) => Automaton s i o -> CompiledAutomaton s i o
compileAutomaton auto = CompiledAutomaton
  { initState  = initialState auto
  , transition = trans
  , final      = finalStates
  }
  where
    trans = M.fromList [ ((s,i),(s',o)) | (s,i,s',o) <- transitions auto ]
    finalStates = S.fromList (accepting auto)

data Process state input output = Process
  { currentState :: state
  , automaton    :: CompiledAutomaton state input output
  } deriving (Eq)

instance Show s => Show (Process s i o) where
  show = show . currentState

start :: CompiledAutomaton state input output -> Process state input output
{-# INLINE start #-}
start auto = Process
  { currentState = initState auto
  , automaton    = auto
  }

restart :: Process s i o -> Process s i o
{-# INLINE restart #-}
restart p = start (automaton p)

runProcess :: (Ord s,Ord i) => i -> Process s i o -> Maybe (o,Process s i o)
{-# INLINE runProcess #-}
runProcess i p = do
  let s = currentState p
  (s',o) <- M.lookup (s,i) $ transition $ automaton p
  return (o,p { currentState = s' })

finished :: Ord s => Process s i o -> Bool
{-# INLINE finished #-}
finished p = currentState p `S.member` final (automaton p)
  
toDot :: (Show d, Show i, Show o, Ord i, Ord d)
      => Automaton (Map d L) i o -> Text
toDot auto = T.unlines $
  ["digraph G {"] ++ stateToDot ++ transitionsToDot ++ ["}"]
  where
    state s = T.pack ("S" ++ show (stateEncoding s))
    tshow :: Show x => x -> Text
    tshow = T.filter (/= '\"') . T.replace "fromList " "" . T.pack . show
    sts = M.fromList (zip (states auto) [(1::Int) ..])
    stateEncoding s = sts M.! s
    trans = M.fromList [ ((s,i),(s',o)) | (s,i,s',o) <- transitions auto ]

    transitionsToDot = do
      ((s,i),(s',o)) <- M.toList trans
      return $ T.unwords [ state s, "->", state s', T.concat ["[ label = \"", tshow i, ",", tshow o, "\" ];"]]

    stateToDot = do
      s <- states auto
      return $ T.unwords [ state s,
                  T.concat [ "["
                           , "label = \"", (T.pack (show (M.toList s))), "\", "
                           , "shape = box"
                           , "];"]]
  
-- toDot' :: (Show s, Show i,Show o) => CompiledAutomaton s i o -> Text
-- toDot' auto = T.unlines $
--   ["digraph G {"] ++ transitionsToDot auto ++ ["}"]
-- 
-- transitionsToDot' :: (Show s, Show i,Show o) => CompiledAutomaton s i o -> [Text]
-- transitionsToDot' auto = do
--   ((s,i),(s',o)) <- M.toList (transition auto)
--   return $ T.unwords [ state s, " -> ", state s', T.concat ["[ label = \"", tshow i, ",", tshow o, "\" ]"]]
--   where
--     state s = T.pack ("S" ++ show s)
--     tshow :: Show x => x -> Text
--     tshow = T.filter (/= '\"') . T.replace "fromList " "" . T.pack . show
