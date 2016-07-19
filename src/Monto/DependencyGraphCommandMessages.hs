module Monto.DependencyGraphCommandMessages where

import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Set             (Set)
import qualified Data.Set             as S

import           Monto.CommandMessage (CommandMessage)
import           Monto.Types

data DependencyGraphCommandMessages
  = DependencyGraphCommandMessages
  { cmdDeps :: Map CommandMessage (Set (Source,ServiceID,Product,Language))
  , depCmds :: Map (Source,ServiceID,Product,Language) (Set CommandMessage)
  } deriving (Eq,Show)

empty :: DependencyGraphCommandMessages
empty = DependencyGraphCommandMessages
  { cmdDeps = M.empty
  , depCmds = M.empty
  }

addDependency :: CommandMessage -> [(Source,ServiceID,Product,Language)] -> DependencyGraphCommandMessages -> DependencyGraphCommandMessages
addDependency cmdMsg depsList graph =
  let cleanedGraph = removeCommandMessage cmdMsg graph
      deps = S.fromList depsList
  in DependencyGraphCommandMessages
    { cmdDeps   = M.insertWith S.union cmdMsg deps (cmdDeps cleanedGraph)
    , depCmds   = foldl (\acc cur -> M.insertWith S.union cur (S.singleton cmdMsg) acc) (depCmds cleanedGraph) deps
    }

removeCommandMessage :: CommandMessage -> DependencyGraphCommandMessages -> DependencyGraphCommandMessages
removeCommandMessage cmdMsg graph =
  case M.lookup cmdMsg (cmdDeps graph) of
    Nothing -> graph
    Just depsWithCmdMsg ->
      DependencyGraphCommandMessages
      { cmdDeps = M.delete cmdMsg (cmdDeps graph)
      , depCmds = S.foldl (\acc cur -> M.update (setDeleteMaybe cmdMsg) cur acc) (depCmds graph) depsWithCmdMsg
      }

setDeleteMaybe :: CommandMessage -> Set CommandMessage -> Maybe (Set CommandMessage)
setDeleteMaybe cmdMsg set =
  let newSet = S.delete cmdMsg set
  in if S.null newSet then Nothing else Just newSet

lookupCommandMessageDependencies :: CommandMessage -> DependencyGraphCommandMessages -> [(Source,ServiceID,Product,Language)]
lookupCommandMessageDependencies cmdMsg graph =
  case M.lookup cmdMsg (cmdDeps graph) of
    Nothing -> []
    Just deps -> S.toList deps

lookupDependencyCommandMessages :: (Source,ServiceID,Product,Language) -> DependencyGraphCommandMessages -> [CommandMessage]
lookupDependencyCommandMessages dep graph =
  case M.lookup dep (depCmds graph) of
    Nothing -> []
    Just cmdMsgs -> S.toList cmdMsgs
