{-# LANGUAGE OverloadedStrings #-}
module DependencyGraphCommandMessagesSpec(spec) where

import qualified Data.Map                             as M
import qualified Data.Set                             as S

import           Monto.CommandMessage
import           Monto.DependencyGraphCommandMessages as DGCM
import qualified Monto.Request                        as Req
import           Monto.Source
import           Monto.SourceMessage
import           Monto.Types

import           Test.Hspec

spec :: Spec
spec = do
  let langJava = "java" :: Language
      langPython = "python" :: Language

      source1 = Source "s1" Nothing
      source2 = Source "s2" Nothing

      productCompletions = "completions" :: Product
      productSource = "source" :: Product

      serviceSource = "source" :: ServiceID
      serviceCodeCompletion = "CodeCompletion" :: ServiceID
      serviceParser = "parser" :: ServiceID

      cmdMsg1 = CommandMessage 1 1 "cmd1" langJava "" []
      cmdMsg2 = CommandMessage 2 1 "cmd2" langJava "" []
      cmdMsg3 = CommandMessage 3 1 "cmd3" langJava "" []

  context "Insertion" $ do
    it "should insert new dependencies" $
      DGCM.addDependency cmdMsg1 [(source1,serviceSource,productSource,langJava)] DGCM.empty `shouldBe`
        DGCM.DependencyGraphCommandMessages
        { cmdDeps = M.fromList [(cmdMsg1,S.fromList [(source1,serviceSource,productSource,langJava)])]
        , depCmds = M.fromList [((source1,serviceSource,productSource,langJava),S.fromList [cmdMsg1])]
        }

    it "should insert multiple dependencies per CommandMessage" $
      DGCM.addDependency cmdMsg1 [(source1,serviceSource,productSource,langJava),
                                  (source2,serviceCodeCompletion,productCompletions,langPython)]
        DGCM.empty `shouldBe`
          DGCM.DependencyGraphCommandMessages
          { cmdDeps = M.fromList [(cmdMsg1,S.fromList [(source1,serviceSource,productSource,langJava),
                                                       (source2,serviceCodeCompletion,productCompletions,langPython)])]
          , depCmds = M.fromList [((source1,serviceSource,productSource,langJava),               S.fromList [cmdMsg1]),
                                  ((source2,serviceCodeCompletion,productCompletions,langPython),S.fromList [cmdMsg1])]
          }

    it "should insert dependencies only once" $
      DGCM.addDependency cmdMsg1 [(source1,serviceSource,productSource,langJava)] (
         DGCM.addDependency cmdMsg1 [(source1,serviceSource,productSource,langJava)]
            DGCM.empty
      ) `shouldBe`
        DGCM.DependencyGraphCommandMessages
        { cmdDeps = M.fromList [(cmdMsg1,S.fromList [(source1,serviceSource,productSource,langJava)])]
        , depCmds = M.fromList [((source1,serviceSource,productSource,langJava),S.fromList [cmdMsg1])]
        }

    it "should store dependencies for multiple CommandMessages" $
      DGCM.addDependency cmdMsg1 [(source1,serviceSource,productSource,langJava)] (
        DGCM.addDependency cmdMsg2 [(source2,serviceCodeCompletion,productCompletions,langPython)]
            DGCM.empty
      ) `shouldBe`
        DGCM.DependencyGraphCommandMessages
        { cmdDeps = M.fromList [(cmdMsg1,S.fromList [(source1,serviceSource,productSource,langJava)]),
                                (cmdMsg2,S.fromList [(source2,serviceCodeCompletion,productCompletions,langPython)])]
        , depCmds = M.fromList [((source1,serviceSource,productSource,langJava),                S.fromList [cmdMsg1]),
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
        , depCmds = M.fromList [((source2,serviceCodeCompletion,productCompletions,langPython), S.fromList [cmdMsg1,cmdMsg2])]
        }

    it "should override old dependencies if the same CommandMessage is added multiple times" $
      DGCM.addDependency cmdMsg1 [(source2,serviceCodeCompletion,productCompletions,langJava)] (
        DGCM.addDependency cmdMsg1 [(source1,serviceCodeCompletion,productCompletions,langPython),
                                    (source1,serviceSource,productSource,langJava)]
          DGCM.empty
      ) `shouldBe`
        DGCM.DependencyGraphCommandMessages
        { cmdDeps = M.fromList [(cmdMsg1,S.fromList [(source2,serviceCodeCompletion,productCompletions,langJava)])]
        , depCmds = M.fromList [((source2,serviceCodeCompletion,productCompletions,langJava), S.fromList [cmdMsg1])]
        }


  context "Deletion" $ do
    it "should delete a single CommandMessage" $
      DGCM.removeCommandMessage cmdMsg1 (
        DGCM.addDependency cmdMsg1 [(source1,serviceSource,productSource,langJava)]
            DGCM.empty
      ) `shouldBe`
        DGCM.empty

    it "should delete multiple dependencies of one CommandMessage" $
      DGCM.removeCommandMessage cmdMsg1 (
        DGCM.addDependency cmdMsg1 [(source1,serviceSource,productSource,langJava),
                                    (source2,serviceCodeCompletion,productCompletions,langPython)]
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
        , depCmds = M.fromList [((source1,serviceSource,productSource,langJava), S.fromList [cmdMsg2])]
        }

    it "should delete multiple dependencies of a CommandMessage" $
      DGCM.removeCommandMessage cmdMsg2 (
        DGCM.addDependency cmdMsg1 [(source1,serviceSource,productSource,langJava)] (
          DGCM.addDependency cmdMsg2 [(source1,serviceSource,productSource,langJava)]
            DGCM.empty
      )) `shouldBe`
        DGCM.DependencyGraphCommandMessages
        { cmdDeps = M.fromList [(cmdMsg1,S.fromList [(source1,serviceSource,productSource,langJava)])]
        , depCmds = M.fromList [((source1,serviceSource,productSource,langJava), S.fromList [cmdMsg1])]
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
        , depCmds = M.fromList [((source1,serviceSource,productSource,langJava), S.fromList [cmdMsg1,cmdMsg2])]
        }

  context "CommandMessage identity" $ do
    it "should identify a CommandMessage based on session, id, command and language (positive)" $
      -- if session,id,command,language are the same on add and remove, the CommandMessage should be deleted
      DGCM.removeCommandMessage (CommandMessage 1 1 "cmdDoStuff" langJava "content" []) (
        DGCM.addDependency (CommandMessage 1 1 "cmdDoStuff" langJava "content" []) [(source1,serviceSource,productSource,langJava)]
          DGCM.empty
      ) `shouldBe`
        DGCM.empty

    it "should identify a CommandMessage based on session, id, command and language (negative 1)" $
      -- if session is different on add and remove, the CommandMessage shouldn't be deleted
      DGCM.removeCommandMessage (CommandMessage 2 1 "cmdDoStuff" langJava "content" []) (
        DGCM.addDependency (CommandMessage 1 1 "cmdDoStuff" langJava "content" []) [(source1,serviceSource,productSource,langJava)]
          DGCM.empty
      ) `shouldBe`
        DGCM.DependencyGraphCommandMessages
        { cmdDeps = M.fromList [(CommandMessage 1 1 "cmdDoStuff" langJava "content" [],S.fromList [(source1,serviceSource,productSource,langJava)])]
        , depCmds = M.fromList [((source1,serviceSource,productSource,langJava), S.fromList [CommandMessage 1 1 "cmdDoStuff" langJava "content" []])]
        }

    it "should identify a CommandMessage based on session, id, command and language (negative 2)" $
      -- if id is different on add and remove, the CommandMessage shouldn't be deleted
      DGCM.removeCommandMessage (CommandMessage 1 2 "cmdDoStuff" langJava "content" []) (
        DGCM.addDependency (CommandMessage 1 1 "cmdDoStuff" langJava "content" []) [(source1,serviceSource,productSource,langJava)]
          DGCM.empty
      ) `shouldBe`
        DGCM.DependencyGraphCommandMessages
        { cmdDeps = M.fromList [(CommandMessage 1 1 "cmdDoStuff" langJava "content" [],S.fromList [(source1,serviceSource,productSource,langJava)])]
        , depCmds = M.fromList [((source1,serviceSource,productSource,langJava), S.fromList [CommandMessage 1 1 "cmdDoStuff" langJava "content" []])]
        }

    it "should identify a CommandMessage based on session, id, command and language (negative 3)" $
      -- if command is different on add and remove, the CommandMessage shouldn't be deleted
      DGCM.removeCommandMessage (CommandMessage 1 1 "cmdDoStuff2" langJava "content" []) (
        DGCM.addDependency (CommandMessage 1 1 "cmdDoStuff" langJava "content" []) [(source1,serviceSource,productSource,langJava)]
          DGCM.empty
      ) `shouldBe`
        DGCM.DependencyGraphCommandMessages
        { cmdDeps = M.fromList [(CommandMessage 1 1 "cmdDoStuff" langJava "content" [],S.fromList [(source1,serviceSource,productSource,langJava)])]
        , depCmds = M.fromList [((source1,serviceSource,productSource,langJava), S.fromList [CommandMessage 1 1 "cmdDoStuff" langJava "content" []])]
        }

    it "should identify a CommandMessage based on session, id, command and language (negative 4)" $
      -- if language is different on add and remove, the CommandMessage shouldn't be deleted
      DGCM.removeCommandMessage (CommandMessage 1 1 "cmdDoStuff" langPython "content" []) (
        DGCM.addDependency (CommandMessage 1 1 "cmdDoStuff" langJava "content" []) [(source1,serviceSource,productSource,langJava)]
          DGCM.empty
      ) `shouldBe`
        DGCM.DependencyGraphCommandMessages
        { cmdDeps = M.fromList [(CommandMessage 1 1 "cmdDoStuff" langJava "content" [],S.fromList [(source1,serviceSource,productSource,langJava)])]
        , depCmds = M.fromList [((source1,serviceSource,productSource,langJava), S.fromList [CommandMessage 1 1 "cmdDoStuff" langJava "content" []])]
        }

    it "shouldn't identify a CommandMessage based on contents" $
      -- if contents is different on add and delete, the CommandMessage should still be deleted
      DGCM.removeCommandMessage (CommandMessage 1 1 "cmdDoStuff" langJava "content1" []) (
        DGCM.addDependency (CommandMessage 1 1 "cmdDoStuff" langJava "content2" []) [(source1,serviceSource,productSource,langJava)]
          DGCM.empty
      ) `shouldBe`
        DGCM.empty

    it "shouldn't identify a CommandMessage based on requirements" $
      -- if requirements is different on add and delete, the CommandMessage should still be deleted
      DGCM.removeCommandMessage (CommandMessage 1 1 "cmdDoStuff" langJava "content" [Req.SourceMessage (SourceMessage (VersionID 1) (Source "sourceFile2" Nothing) langJava "source code")]) (
        DGCM.addDependency (CommandMessage 1 1 "cmdDoStuff" langJava "content" [Req.SourceMessage (SourceMessage (VersionID 1) (Source "sourceFile1" Nothing) langJava "source code")]) [(source1,serviceSource,productSource,langJava)]
          DGCM.empty
      ) `shouldBe`
        DGCM.empty
