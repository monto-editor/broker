{-# LANGUAGE OverloadedStrings #-}
module DependencyGraphCommandMessagesSpec(spec) where

import qualified Data.Map                            as M
import qualified Data.Set                            as S

import qualified Monto.CommandMessage                as CM
import           Monto.DependencyGraphCommandMessages as DGCM
import           Monto.Types

import           Test.Hspec

spec :: Spec
spec = do
  let langJava = "java" :: Language
      langPython = "python" :: Language

      source1 = "s1" :: Source
      source2 = "s2" :: Source

      productCompletions = "completions" :: Product
      productSource = "source" :: Product

      serviceSource = "source" :: ServiceID
      serviceCodeCompletion = "CodeCompletion" :: ServiceID
      serviceParser = "parser" :: ServiceID

      cmdMsg1 = CM.CommandMessage 1 1 serviceParser "cmd1" ""
      cmdMsg2 = CM.CommandMessage 2 1 serviceCodeCompletion "cmd2" ""
      cmdMsg3 = CM.CommandMessage 3 1 serviceParser "cmd3" ""

  context "Insertion" $ do
    it "should insert new dependencies" $
      DGCM.addDependency cmdMsg1 [(source1,serviceSource,productSource,langJava)] DGCM.empty `shouldBe`
        DGCM.DependencyGraphCommandMessages
        { cmdDeps = M.fromList [(cmdMsg1,S.fromList [(source1,serviceSource,productSource,langJava)])]
        , depsCmd = M.fromList [((source1,serviceSource,productSource,langJava),S.fromList [cmdMsg1])]
        }

    it "should insert dependencies only once" $
      DGCM.addDependency cmdMsg1 [(source1,serviceSource,productSource,langJava)] (
         DGCM.addDependency cmdMsg1 [(source1,serviceSource,productSource,langJava)]
            DGCM.empty
      ) `shouldBe`
        DGCM.DependencyGraphCommandMessages
        { cmdDeps = M.fromList [(cmdMsg1,S.fromList [(source1,serviceSource,productSource,langJava)])]
        , depsCmd = M.fromList [((source1,serviceSource,productSource,langJava),S.fromList [cmdMsg1])]
        }

    it "should store multiple dependencies per CommandMessage correctly" $
      DGCM.addDependency cmdMsg1 [(source1,serviceSource,productSource,langJava)] (
        DGCM.addDependency cmdMsg1 [(source2,serviceCodeCompletion,productCompletions,langPython)]
            DGCM.empty
      ) `shouldBe`
        DGCM.DependencyGraphCommandMessages
        { cmdDeps = M.fromList [(cmdMsg1,S.fromList [(source1,serviceSource,productSource,langJava),
                                                     (source2,serviceCodeCompletion,productCompletions,langPython)])]
        , depsCmd = M.fromList [((source1,serviceSource,productSource,langJava),                S.fromList [cmdMsg1]),
                                ((source2,serviceCodeCompletion,productCompletions,langPython), S.fromList [cmdMsg1])]
        }

    it "should store dependencies for multiple CommandMessages" $
      DGCM.addDependency cmdMsg1 [(source1,serviceSource,productSource,langJava)] (
        DGCM.addDependency cmdMsg2 [(source2,serviceCodeCompletion,productCompletions,langPython)]
            DGCM.empty
      ) `shouldBe`
        DGCM.DependencyGraphCommandMessages
        { cmdDeps = M.fromList [(cmdMsg1,S.fromList [(source1,serviceSource,productSource,langJava)]),
                                (cmdMsg2,S.fromList [(source2,serviceCodeCompletion,productCompletions,langPython)])]
        , depsCmd = M.fromList [((source1,serviceSource,productSource,langJava),                S.fromList [cmdMsg1]),
                                ((source2,serviceCodeCompletion,productCompletions,langPython), S.fromList [cmdMsg2])]
        }

    it "should store the same dependency for different CommandMessages" $
      DGCM.addDependency cmdMsg1 [(source2,serviceCodeCompletion,productCompletions,langPython)] (
        DGCM.addDependency cmdMsg2 [(source2,serviceCodeCompletion,productCompletions,langPython)]
            DGCM.empty
      ) `shouldBe`
        DGCM.DependencyGraphCommandMessages
        { cmdDeps = M.fromList [(cmdMsg1,S.fromList [(source2,serviceCodeCompletion,productCompletions,langPython)]),
                                (cmdMsg2,S.fromList [(source2,serviceCodeCompletion,productCompletions,langPython)])]
        , depsCmd = M.fromList [((source2,serviceCodeCompletion,productCompletions,langPython), S.fromList [cmdMsg1,cmdMsg2])]
        }

  context "Deletion" $ do
    it "should delete a single CommandMessage" $
      DGCM.removeCommandMessage cmdMsg1 (
        DGCM.addDependency cmdMsg1 [(source1,serviceSource,productSource,langJava)]
            DGCM.empty
      ) `shouldBe`
        DGCM.empty

    it "should delete only the correct CommandMessage" $
      DGCM.removeCommandMessage cmdMsg1 (
        DGCM.addDependency cmdMsg1 [(source1,serviceSource,productSource,langJava)] (
          DGCM.addDependency cmdMsg2 [(source1,serviceSource,productSource,langJava)]
            DGCM.empty
      )) `shouldBe`
        DGCM.DependencyGraphCommandMessages
        { cmdDeps = M.fromList [(cmdMsg2,S.fromList [(source1,serviceSource,productSource,langJava)])]
        , depsCmd = M.fromList [((source1,serviceSource,productSource,langJava), S.fromList [cmdMsg2])]
        }

    it "should delete multiple dependencies of a CommandMessage" $
      DGCM.removeCommandMessage cmdMsg2 (
        DGCM.addDependency cmdMsg1 [(source1,serviceSource,productSource,langJava)] (
          DGCM.addDependency cmdMsg2 [(source1,serviceSource,productSource,langJava)] (
            DGCM.addDependency cmdMsg2 [(source2,serviceCodeCompletion,productCompletions,langPython)]
              DGCM.empty
      ))) `shouldBe`
        DGCM.DependencyGraphCommandMessages
        { cmdDeps = M.fromList [(cmdMsg1,S.fromList [(source1,serviceSource,productSource,langJava)])]
        , depsCmd = M.fromList [((source1,serviceSource,productSource,langJava), S.fromList [cmdMsg1])]
        }

    it "should delete multiple dependencies of a CommandMessage 2" $
      DGCM.removeCommandMessage cmdMsg2 (
        DGCM.addDependency cmdMsg1 [(source1,serviceSource,productSource,langJava)] (
          DGCM.addDependency cmdMsg2 [(source1,serviceSource,productSource,langJava)] (
            DGCM.addDependency cmdMsg1 [(source2,serviceCodeCompletion,productCompletions,langPython)]
              DGCM.empty
      ))) `shouldBe`
        DGCM.DependencyGraphCommandMessages
        { cmdDeps = M.fromList [(cmdMsg1,S.fromList [(source1,serviceSource,productSource,langJava),
                                                     (source2,serviceCodeCompletion,productCompletions,langPython)])]
        , depsCmd = M.fromList [((source1,serviceSource,productSource,langJava),                S.fromList [cmdMsg1]),
                                ((source2,serviceCodeCompletion,productCompletions,langPython), S.fromList [cmdMsg1])]
        }

    it "should not change if a not added CommandMessage is deleted" $
      DGCM.removeCommandMessage cmdMsg3 (
        DGCM.addDependency cmdMsg1 [(source1,serviceSource,productSource,langJava)] (
          DGCM.addDependency cmdMsg2 [(source1,serviceSource,productSource,langJava)] 
            DGCM.empty
      )) `shouldBe`
        DGCM.DependencyGraphCommandMessages
        { cmdDeps = M.fromList [(cmdMsg1,S.fromList [(source1,serviceSource,productSource,langJava)]),
                                (cmdMsg2,S.fromList [(source1,serviceSource,productSource,langJava)])]
        , depsCmd = M.fromList [((source1,serviceSource,productSource,langJava), S.fromList [cmdMsg1,cmdMsg2])]
        }