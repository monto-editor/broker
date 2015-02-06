{-# LANGUAGE OverloadedStrings #-}
module Monto.Automaton.Compiled where

import           Control.Applicative

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import           Data.Text (Text)
import qualified Data.Text as T

import           Monto.Automaton (Automaton,states,transitions,initialState,accepting)

data CompiledAutomaton i o = CompiledAutomaton
  { initState  :: Int
  , transition :: Map (Int,i) (Int,o)
  , final      :: IntSet
  } deriving (Eq,Show)

compileAutomaton :: (Ord s,Ord i) => Automaton s i o -> CompiledAutomaton i o
compileAutomaton auto = CompiledAutomaton
  { initState  = stateEncoding (initialState auto)
  , transition = trans
  , final      = finalStates
  }
  where
    sts = M.fromList (zip (states auto) [1..])
    stateEncoding s = sts M.! s
    trans = M.fromList [ ((stateEncoding s,i),(stateEncoding s',o)) | (s,i,s',o) <- transitions auto ]
    finalStates = IS.fromList (stateEncoding <$> accepting auto)

empty :: CompiledAutomaton i o
empty = CompiledAutomaton
  { initState  = 0
  , transition = M.empty
  , final      = IS.singleton 0
  }

data Process i o = Process
  { currentState :: Int
  , automaton    :: CompiledAutomaton i o
  } deriving (Eq,Show)

start :: CompiledAutomaton i o -> Process i o
{-# INLINE start #-}
start auto = Process
  { currentState = initState auto
  , automaton    = auto
  }

restart :: Process i o -> Process i o
{-# INLINE restart #-}
restart p = start (automaton p)

runProcess :: Ord i => i -> Process i o -> Maybe (o,Process i o)
{-# INLINE runProcess #-}
runProcess i p = do
  let s = currentState p
  (s',o) <- M.lookup (s,i) $ transition $ automaton p
  return (o,p { currentState = s' })

finished :: Process i o -> Bool
{-# INLINE finished #-}
finished p = currentState p `IS.member` final (automaton p)
  
toDot :: (Show i,Show o) => CompiledAutomaton i o -> Text
toDot auto = T.unlines $
  ["digraph G {"] ++ transitionsToDot auto ++ ["}"]

transitionsToDot :: (Show i,Show o) => CompiledAutomaton i o -> [Text]
transitionsToDot auto = do
  ((s,i),(s',o)) <- M.toList (transition auto)
  return $ T.unwords [ state s, " -> ", state s', T.concat ["[ label = \"", tshow i, ",", tshow o, "\" ]"]]
  where
    state s = T.pack ("S" ++ show s)
    tshow :: Show x => x -> Text
    tshow = T.filter (/= '\"') . T.replace "fromList " "" . T.pack . show
