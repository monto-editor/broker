module Monto.DependencyGraphCommandMessages where

import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Set              (Set)
import qualified Data.Set              as S

import           Monto.CommandMessage  (CommandMessage)
import           Monto.Types

data DependencyGraphCommandMessages
  = DependencyGraphCommandMessages
  { cmdDeps :: Map CommandMessage (Set (Source,ServiceID,Product,Language))
  , depsCmd :: Map (Source,ServiceID,Product,Language) (Set CommandMessage)
  } deriving (Eq,Show)

empty :: DependencyGraphCommandMessages
empty = DependencyGraphCommandMessages
  { cmdDeps = M.empty
  , depsCmd = M.empty
  }

addDependency :: CommandMessage -> [(Source,ServiceID,Product,Language)] -> DependencyGraphCommandMessages -> DependencyGraphCommandMessages
addDependency cmdMsg depsList graph =
  let deps = S.fromList depsList
  in DependencyGraphCommandMessages -- TODO: why in Broker always `in broker {}`?
    { cmdDeps   = M.insertWith S.union cmdMsg deps (cmdDeps graph)
    , depsCmd   = foldl (\acc cur -> M.insertWith S.union cur (S.singleton cmdMsg) acc) (depsCmd graph) deps
    }

removeCommandMessage :: CommandMessage -> DependencyGraphCommandMessages -> DependencyGraphCommandMessages
removeCommandMessage cmdMsg graph =
  case M.lookup cmdMsg (cmdDeps graph) of
    Nothing -> graph
    Just depsWithCmdMsg ->
      DependencyGraphCommandMessages
      { cmdDeps = M.delete cmdMsg (cmdDeps graph)
      , depsCmd = S.foldl (\acc cur -> M.update (setDifferenceMaybe cmdMsg) cur acc) (depsCmd graph) depsWithCmdMsg
      }

setDifferenceMaybe :: CommandMessage -> Set CommandMessage -> Maybe (Set CommandMessage)
setDifferenceMaybe cmdMsg set =
  let newSet = S.delete cmdMsg set
  in if S.null newSet then Nothing else Just newSet
