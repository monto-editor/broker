{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module BrokerSpec(spec) where

import           Control.Monad.State

import           Data.Aeson                               (toJSON)
import qualified Data.Set                                 as S
import qualified Data.Map                                 as M

import qualified Monto.Broker                             as B
import           Monto.CommandMessage
import           Monto.CommandDescription
import           Monto.DynamicDependency
import           Monto.ProductDependency
import           Monto.ProductDescription
import           Monto.ProductMessage
import           Monto.RegisterCommandMessageDependencies
import           Monto.RegisterDynamicDependencies
import           Monto.RegisterServiceRequest
import           Monto.Request                            (Request (Request))
import qualified Monto.Request                            as Req
import           Monto.Source
import           Monto.SourceMessage
import           Monto.Types

import           Test.Hspec


spec :: Spec
spec = do
      -- Global
  let register sid prods deps = B.registerService (RegisterServiceRequest sid "" "" Nothing prods deps [])
      registerCmds sid cmds = B.registerService (RegisterServiceRequest sid "" "" Nothing [] [] cmds)
      java = "java" :: Language
      python = "python" :: Language
      tokens = "tokens" :: Product
      ast = "ast" :: Product
      identifiers = "identifiers" :: Product
      completions = "completions" :: Product
      errors = "errors" :: Product
      sourceProduct = "source" :: Product
      sourceService = "source" :: ServiceID
      v1 = VersionID 1
      v2 = VersionID 2
      s1 = Source "s1" Nothing
      s2 = Source "s2" Nothing
      s3 = Source "s3" Nothing
      s20 = Source "s20" Nothing
      s21 = Source "s21" Nothing

      -- Java
      javaTypechecker = "javaTypechecker" :: ServiceID
      javaCodeCompletion = "javaCodeCompletion" :: ServiceID
      javaParser = "javaParser" :: ServiceID
      javaTokenizer = "javaTokenizer" :: ServiceID
      javaSource = ProductDependency "source" "source" java
      javaS1 vid = SourceMessage vid s1 java ""
      javaS2 vid = SourceMessage vid s2 java ""
      javaS3 vid = SourceMessage vid s3 java ""
      javaAstMsg vid src = ProductMessage vid src javaParser ast java "" (toJSON (0::Int))
      javaTypeMsg vid src = ProductMessage vid src javaTypechecker errors java "" (toJSON (0::Int))
      javaCodeCMsg vid src = ProductMessage vid src javaCodeCompletion completions java "" (toJSON (0::Int))
      javaTokenMsg vid src = ProductMessage vid src javaTokenizer tokens java "" (toJSON (0::Int))

      -- Python
      pythonS20 i = SourceMessage i s20 python ""
      pythonS21 i = SourceMessage i s21 python ""
      pythonParser = "pythonParser" :: ServiceID
      pythonIdentifierFinder = "pythonIdentifierFinder" :: ServiceID
      pythonCodeCompletion = "pythonCodeCompletion" :: ServiceID
      pythonSource = ProductDependency "source" "source" python
      pythonAstMsg vid src = ProductMessage vid src pythonParser ast python "" (toJSON (0::Int))
      pythonIdentifiersMsg vid src = ProductMessage vid src pythonIdentifierFinder identifiers python "" (toJSON (0::Int))
      pythonSourceMsg vid src = SourceMessage vid src python ""

  context "Service management" $

    it "can handle services of two different languages" $ do
          let broker = register pythonParser [ProductDescription ast python] [pythonSource]
                     $ register javaParser [ProductDescription ast java] [javaSource]
                     $ B.empty (Port 5010) (Port 5020)
          void $ flip execStateT broker $ do
            -- Arrival of sm of python s20 should only generate request for python parser for s20
            B.newVersion (pythonS20 v1) `shouldBeAsSetTuple`
              ([Request s20 pythonParser [Req.SourceMessage (pythonS20 v1)]], [])

            -- Arrival of sm of java s1 should only generate request for java for s1
            B.newVersion (javaS1 v1) `shouldBeAsSetTuple`
              ([Request s1 javaParser [Req.SourceMessage (javaS1 v1)]], [])

  context "CommandConsumer" $ do 

    it "can handle one CommandConsumer from one service" $
      B.commandConsumers (
        registerCmds javaParser [CommandDescription "cmd1" java] 
          (B.empty (Port 5010) (Port 5020))) `shouldBe`
            M.fromList [(CommandDescription "cmd1" java, [javaParser])]

    it "can handle two CommandConsumers from one service" $
      B.commandConsumers (
        registerCmds javaParser [CommandDescription "cmd1" java, CommandDescription "cmd2" java]
          (B.empty (Port 5010) (Port 5020))) `shouldBe`
            M.fromList [(CommandDescription "cmd1" java, [javaParser]),
                        (CommandDescription "cmd2" java, [javaParser])]

    it "can handle two CommandConsumers from one service" $
      B.commandConsumers (
        registerCmds javaParser [CommandDescription "cmd1" java, CommandDescription "cmd1" python]
          (B.empty (Port 5010) (Port 5020))) `shouldBe`
            M.fromList [(CommandDescription "cmd1" java, [javaParser]),
                        (CommandDescription "cmd1" python, [javaParser])]


    it "can handle distinct CommandConsumers from multiple services" $
      B.commandConsumers (
        registerCmds javaParser [CommandDescription "cmd1" java] (
          registerCmds javaCodeCompletion [CommandDescription "cmd2" java]
            (B.empty (Port 5010) (Port 5020)))) `shouldBe`
              M.fromList [(CommandDescription "cmd1" java, [javaParser]),
                          (CommandDescription "cmd2" java, [javaCodeCompletion])]

    it "can handle the same CommandConsumer from multiple services" $
      B.commandConsumers (
        registerCmds javaParser [CommandDescription "cmd1" python] (
          registerCmds javaCodeCompletion [CommandDescription "cmd1" python]
            (B.empty (Port 5010) (Port 5020)))) `shouldBe`
              M.fromList [(CommandDescription "cmd1" python, [javaParser, javaCodeCompletion])]

  context "Product dependencies" $ do

    let broker = register javaCodeCompletion [ProductDescription completions java] [javaSource, ProductDependency javaParser ast java]
               $ register javaTypechecker [ProductDescription errors java] [javaSource, ProductDependency javaParser ast java]
               $ register javaParser [ProductDescription ast java] [javaSource]
               $ register javaTokenizer [ProductDescription tokens java] [javaSource]
               $ B.empty (Port 5010) (Port 5020)

    it "can manage static server dependencies" $
      void $ flip execStateT broker $ do

        B.newVersion (javaS1 v1) `shouldBeAsSetTuple` ([
          Request s1 javaParser [Req.SourceMessage (javaS1 v1)],
          Request s1 javaTokenizer [Req.SourceMessage (javaS1 v1)]
          ], [])

        let ast1 = javaAstMsg v1 s1
        B.newProduct ast1 `shouldBeAsSetTuple` ([
          Request s1 javaCodeCompletion [Req.SourceMessage (javaS1 v1), Req.ProductMessage ast1],
          Request s1 javaTypechecker [Req.SourceMessage (javaS1 v1), Req.ProductMessage ast1]
          ], [])

        B.newProduct (javaTokenMsg v1 s1) `shouldBeAsSetTuple` ([], [])
        B.newProduct (javaCodeCMsg v1 s1) `shouldBeAsSetTuple` ([], [])
        B.newProduct (javaTypeMsg v1 s1) `shouldBeAsSetTuple` ([], [])

    it "can deal with outdated products" $ do
      _ <- flip execStateT broker $ do
        let currentId = VersionID 42
            outdatedId = VersionID 41

        B.newVersion (javaS1 currentId) `shouldBeAsSetTuple` ([
          Request s1 javaParser [Req.SourceMessage (javaS1 currentId)],
          Request s1 javaTokenizer [Req.SourceMessage (javaS1 currentId)]
          ], [])

        -- outdated product arrives
        B.newProduct (javaAstMsg outdatedId s1) `shouldBeAsSetTuple` ([], [])

      return ()

  context "Dynamic dependencies" $ do

    it "creates requests for dynamic dependencies only if they have arrived" $ do
      let broker = register pythonCodeCompletion [ProductDescription completions python] [ProductDependency pythonParser ast python]
                 $ B.empty (Port 5010) (Port 5020)

      void $ flip execStateT broker $ do

        B.newDynamicDependency (RegisterDynamicDependencies s20 pythonCodeCompletion [DynamicDependency s20 sourceService sourceProduct python]) `shouldBe'`
          Nothing

        -- Arrival of sm of s20 should generate no codeCompletion requests
        B.newVersion (pythonS20 v1) `shouldBeAsSetTuple`
          ([], [])

        let pythonAstMsgS20 = pythonAstMsg v1 s20

        -- Arrival of ast pm of s20 should generate codeCompletion request for s20
        B.newProduct pythonAstMsgS20 `shouldBeAsSetTuple`
          ([Request s20 pythonCodeCompletion [Req.ProductMessage pythonAstMsgS20, Req.SourceMessage (pythonS20 v1)]], [])

    it "creates requests for new dynamic dependencies instantly, if they are already fulfilled" $ do
      let broker = register pythonCodeCompletion [ProductDescription completions python] []
                 $ B.empty (Port 5010) (Port 5020)
      void $ flip execStateT broker $ do

        -- Arrival of sm of s20 should generate no codeCompletion requests
        B.newVersion (pythonS20 v1) `shouldBeAsSetTuple`
          ([], [])

        -- Registration of dynamic dependency 'code completions service for s20 depends on source of s20' should immediately generate request
        B.newDynamicDependency (RegisterDynamicDependencies s20 pythonCodeCompletion [DynamicDependency s20 sourceService sourceProduct python]) `shouldBe'`
          Just (Request s20 pythonCodeCompletion [Req.SourceMessage (pythonSourceMsg v1 s20)])

    it "overrides dynamic dependencies, if they are defined twice" $ do
      let broker = register pythonCodeCompletion [] []
                 $ register pythonParser [ProductDescription ast python] [pythonSource]
                 $ B.empty (Port 5010) (Port 5020)
      void $ flip execStateT broker $ do

        -- pythonCodeCompletion has no static dependencies

        -- Register (s20,pythonCodeCompletion) depends on (s20,source)
        B.newDynamicDependency (RegisterDynamicDependencies s20 pythonCodeCompletion [DynamicDependency s20 sourceService sourceProduct python]) `shouldBe'`
          Nothing

        -- But then override (s20,source) with (s20,ast) dependency
        B.newDynamicDependency (RegisterDynamicDependencies s20 pythonCodeCompletion [DynamicDependency s20 pythonParser ast python]) `shouldBe'`
          Nothing

        B.newVersion (pythonS20 v1) `shouldBeAsSetTuple`
          ([Request s20 pythonParser [Req.SourceMessage (pythonS20 v1)]], [])

        -- Arrival of ast of s20 should generate request with only the ast dependency. Source dependency should have been overridden
        B.newProduct (pythonAstMsg v1 s20) `shouldBeAsSetTuple`
          ([Request s20 pythonCodeCompletion [Req.ProductMessage (pythonAstMsg v1 s20)]], [])

  context "CommandMessage dependencies" $ do

    it "should generate a CommandMessage, when its source dependency is fulfilled" $ do
      let broker = register javaParser [ProductDescription ast java] []
                 $ B.empty (Port 5010) (Port 5020)

      void $ flip execStateT broker $ do
        B.newCommandMessageDependency (RegisterCommandMessageDependencies (CommandMessage 1 1 "parserCmd" java "" [])
                                       [DynamicDependency s1 sourceService sourceProduct java]) `shouldBe'`
          Nothing

        B.newVersion (javaS1 v1) `shouldBeAsSetTuple`
          ([],[CommandMessage 1 1 "parserCmd" java "" [Req.SourceMessage (javaS1 v1)]])

    it "should generate a CommandMessage, once all its source dependencies are fulfilled" $ do
      let broker = register javaParser [ProductDescription ast java] []
                 $ B.empty (Port 5010) (Port 5020)

      void $ flip execStateT broker $ do
        B.newCommandMessageDependency (RegisterCommandMessageDependencies (CommandMessage 1 1 "parserCmd" java "" [])
                                       [DynamicDependency s1 sourceService sourceProduct java, DynamicDependency s20 sourceService sourceProduct python]) `shouldBe'`
          Nothing

        B.newVersion (pythonS20 v1) `shouldBeAsSetTuple`
          ([],[])

        B.newVersion (javaS1 v1) `shouldBeAsSetTuple`
          ([],[CommandMessage 1 1 "parserCmd" java "" [Req.SourceMessage (javaS1 v1)]])

    it "should generate a CommandMessage, when its product dependency is fulfilled" $ do
      let broker = register javaParser [ProductDescription ast java] [javaSource]
                 $ register javaCodeCompletion [] []
                 $ B.empty (Port 5010) (Port 5020)
      void $ flip execStateT broker $ do
        B.newCommandMessageDependency (RegisterCommandMessageDependencies (CommandMessage 1 1 "codeCompCmd" java "" [])
                                       [DynamicDependency s1 javaParser ast java]) `shouldBe'`
          Nothing

        B.newVersion (javaS1 v1) `shouldBeAsSetTuple`
          ([Request s1 javaParser [Req.SourceMessage (javaS1 v1)]],[])

        B.newProduct (javaAstMsg v1 s1) `shouldBeAsSetTuple`
          ([], [CommandMessage 1 1 "codeCompCmd" java "" [Req.ProductMessage (javaAstMsg v1 s1)]])

    it "should generate a CommandMessage, once all its product dependencies are fulfilled" $ do
      let broker = register javaParser [ProductDescription ast java] [javaSource]
                 $ register javaTokenizer [ProductDescription tokens java] [javaSource]
                 $ register javaCodeCompletion [] []
                 $ B.empty (Port 5010) (Port 5020)
      void $ flip execStateT broker $ do
        B.newCommandMessageDependency (RegisterCommandMessageDependencies (CommandMessage 1 1 "codeCompCmd" java "" [])
                                       [DynamicDependency s1 javaParser ast java, DynamicDependency s1 javaTokenizer tokens java]) `shouldBe'`
          Nothing

        B.newVersion (javaS1 v1) `shouldBeAsSetTuple`
          ([Request s1 javaParser [Req.SourceMessage (javaS1 v1)],Request s1 javaTokenizer [Req.SourceMessage (javaS1 v1)]],[])

        B.newProduct (javaTokenMsg v1 s1) `shouldBeAsSetTuple`
          ([], [])

        B.newProduct (javaAstMsg v1 s1) `shouldBeAsSetTuple`
          ([], [CommandMessage 1 1 "codeCompCmd" java "" [Req.ProductMessage (javaAstMsg v1 s1), Req.ProductMessage (javaTokenMsg v1 s1)]])

    it "should generate a CommandMessage, once all its mixed dependencies are fulfilled" $ do
      let broker = register javaParser [ProductDescription ast java] [javaSource]
                 $ register javaCodeCompletion [] []
                 $ B.empty (Port 5010) (Port 5020)
      void $ flip execStateT broker $ do
        B.newCommandMessageDependency (RegisterCommandMessageDependencies (CommandMessage 1 1 "codeCompCmd" java "" [])
                                       [DynamicDependency s1 javaParser ast java, DynamicDependency s1 sourceService sourceProduct java]) `shouldBe'`
          Nothing

        B.newVersion (javaS1 v1) `shouldBeAsSetTuple`
          ([Request s1 javaParser [Req.SourceMessage (javaS1 v1)]], [])

        B.newProduct (javaAstMsg v1 s1) `shouldBeAsSetTuple`
          ([], [CommandMessage 1 1 "codeCompCmd" java "" [Req.ProductMessage (javaAstMsg v1 s1), Req.SourceMessage (javaS1 v1)]])

    it "only generate CommandMessages, with the last specified dependencies" $ do
      let broker = register javaParser [ProductDescription ast java] [javaSource]
                 $ register javaTokenizer [ProductDescription tokens java] [javaSource]
                 $ register javaCodeCompletion [] []
                 $ B.empty (Port 5010) (Port 5020)
      void $ flip execStateT broker $ do
        B.newCommandMessageDependency (RegisterCommandMessageDependencies (CommandMessage 1 1 "codeCompCmd" java "" [])
                                       [DynamicDependency s1 javaParser ast java, DynamicDependency s1 javaTokenizer tokens java, DynamicDependency s1 sourceService sourceProduct java]) `shouldBe'`
          Nothing

        B.newCommandMessageDependency (RegisterCommandMessageDependencies (CommandMessage 1 1 "codeCompCmd" java "" [])
                                       [DynamicDependency s1 javaParser ast java, DynamicDependency s1 sourceService sourceProduct java]) `shouldBe'`
          Nothing

        B.newVersion (javaS1 v1) `shouldBeAsSetTuple`
          ([Request s1 javaParser [Req.SourceMessage (javaS1 v1)], Request s1 javaTokenizer [Req.SourceMessage (javaS1 v1)]], [])

        B.newProduct (javaTokenMsg v1 s1) `shouldBeAsSetTuple`
          ([], [])

        B.newProduct (javaAstMsg v1 s1) `shouldBeAsSetTuple`
          ([], [CommandMessage 1 1 "codeCompCmd" java "" [Req.ProductMessage (javaAstMsg v1 s1), Req.SourceMessage (javaS1 v1)]])

    it "not generate the same CommandMessages twice" $ do
      let broker = register javaParser [ProductDescription ast java] []
                 $ B.empty (Port 5010) (Port 5020)

      void $ flip execStateT broker $ do
        B.newCommandMessageDependency (RegisterCommandMessageDependencies (CommandMessage 1 1 "parserCmd" java  "" [])
                                       [DynamicDependency s1 sourceService sourceProduct java]) `shouldBe'`
          Nothing

        B.newVersion (javaS1 v1) `shouldBeAsSetTuple`
          ([],[CommandMessage 1 1 "parserCmd" java "" [Req.SourceMessage (javaS1 v1)]])

        B.newVersion (javaS1 v2) `shouldBeAsSetTuple`
          ([],[])

        B.newVersion (javaS1 v1) `shouldBeAsSetTuple`
          ([],[])

    it "should handle multiple CommandMessages at once" $ do
      let broker = register javaCodeCompletion [] []
                 $ B.empty (Port 5010) (Port 5020)

      void $ flip execStateT broker $ do
        B.newCommandMessageDependency (RegisterCommandMessageDependencies (CommandMessage 1 1 "codeCompCmd" java "" [])
                                       [DynamicDependency s1 sourceService sourceProduct java]) `shouldBe'`
          Nothing

        B.newCommandMessageDependency (RegisterCommandMessageDependencies (CommandMessage 2 1 "codeCompCmd" java "" [])
                                        [DynamicDependency s2 sourceService sourceProduct java]) `shouldBe'`
          Nothing

        B.newVersion (javaS1 v1) `shouldBeAsSetTuple`
          ([],[CommandMessage 1 1 "codeCompCmd" java "" [Req.SourceMessage (javaS1 v1)]])

        B.newVersion (javaS2 v1) `shouldBeAsSetTuple`
          ([],[CommandMessage 2 1 "codeCompCmd" java "" [Req.SourceMessage (javaS2 v1)]])

    it "should generated a CommandMessage if the new CommandMessageDependency is already fulfilled 1" $ do
      -- source dependency
      let broker = register javaCodeCompletion [] []
                 $ B.empty (Port 5010) (Port 5020)

      void $ flip execStateT broker $ do
        B.newVersion (javaS1 v1) `shouldBeAsSetTuple`
          ([],[])

        B.newCommandMessageDependency (RegisterCommandMessageDependencies (CommandMessage 1 1 "codeCompCmd" java "" [])
                                     [DynamicDependency s1 sourceService sourceProduct java]) `shouldBe'`
          Just (CommandMessage 1 1 "codeCompCmd" java "" [Req.SourceMessage (javaS1 v1)])

    it "should generated a CommandMessage if the new CommandMessageDependency is already fulfilled 2" $ do
      -- product dependency
      let broker = register javaParser [ProductDescription ast java] [javaSource]
                 $ register javaCodeCompletion [] []
                 $ B.empty (Port 5010) (Port 5020)

      void $ flip execStateT broker $ do
        B.newVersion (javaS1 v1) `shouldBeAsSetTuple`
          ([Request s1 javaParser [Req.SourceMessage (javaS1 v1)]],[])

        B.newProduct (javaAstMsg v1 s1) `shouldBeAsSetTuple`
          ([],[])

        B.newCommandMessageDependency (RegisterCommandMessageDependencies (CommandMessage 1 1 "codeCompCmd" java "" [])
                                     [DynamicDependency s1 javaParser ast java]) `shouldBe'`
          Just (CommandMessage 1 1 "codeCompCmd" java "" [Req.ProductMessage (javaAstMsg v1 s1)])

    it "should only include the latest version of Messages as dependencies" $ do
      let broker = register javaParser [ProductDescription ast java] []
                 $ B.empty (Port 5010) (Port 5020)

      void $ flip execStateT broker $ do
        B.newVersion (javaS1 v1) `shouldBeAsSetTuple`
          ([],[])

        B.newVersion (javaS1 v2) `shouldBeAsSetTuple`
          ([],[])

        B.newCommandMessageDependency (RegisterCommandMessageDependencies (CommandMessage 1 1 "codeCompCmd" java "" [])
                                     [DynamicDependency s1 sourceService sourceProduct java]) `shouldBe'`
          Just (CommandMessage 1 1 "codeCompCmd" java "" [Req.SourceMessage (javaS1 v2)])

  context "Combined dependencies" $
    it "should generate CommandMessages, product and dynamic dependencies in combination" $ do
      let broker = register pythonIdentifierFinder [ProductDescription identifiers python] [ProductDependency pythonParser ast python]
                 $ register pythonParser [ProductDescription ast python] [pythonSource]
                 $ register javaTypechecker [ProductDescription errors java] [javaSource,ProductDependency javaParser ast java]
                 $ register javaParser [ProductDescription ast java] [javaSource]
                 $ B.empty (Port 5010) (Port 5020)
      void $ flip execStateT broker $ do

        --
        --    s20              s21
        --     ^                ^
        --     |                |
        --    PRO              PRO
        --     |                |
        --    ast  <-- DYN --  ast
        --     ^                ^
        --     |                |
        --    PRO              PRO
        --     |                |
        --   iden  <-- DYN --  iden
        --     ^                ^
        --     |                |
        --    CMD              CMD
        --     |                |
        --   codeC            codeC
        --
        -- PRO = Product Dependency
        -- DYN = Dynamic Dependency
        -- CMD = Command Message Dependency
        --

        -- Arrival of sm of s20 should generate parser request for s20
        B.newVersion (pythonS20 v1) `shouldBeAsSetTuple`
          ([Request s20 pythonParser [Req.SourceMessage (pythonS20 v1)]], [])

        -- Arrival of sm of s21 should generate parser request for s21
        B.newVersion (pythonS21 v1) `shouldBeAsSetTuple`
          ([Request s21 pythonParser [Req.SourceMessage (pythonS21 v1)]], [])

        -- Registration of dynamic dependency 'python parser service for s21 depends on ast of s20' should not generate requests
        B.newDynamicDependency (RegisterDynamicDependencies s21 pythonParser [DynamicDependency s20 pythonParser ast python]) `shouldBe'`
          Nothing

        -- Arrival of sm of s21 should generate no requests, because ast pm of s20 is also required to generate parser request for s21
        B.newVersion (pythonS21 v1) `shouldBeAsSetTuple`
          ([], [])

        let pythonAstMsgS20 = pythonAstMsg v1 s20

        -- Arrival of ast pm of s20 should generate parser request for s21 and identifier request for s20
        B.newProduct pythonAstMsgS20 `shouldBeAsSetTuple` ([
          Request s20 pythonIdentifierFinder [Req.ProductMessage pythonAstMsgS20],
          Request s21 pythonParser [Req.ProductMessage pythonAstMsgS20, Req.SourceMessage (pythonS21 v1)]
          ], [])

        -- Test, that requests for directly fulfilled new dynamic dependencies get generated
        -- First let everything arrive, but identifiers of s21
        let pythonIdentifiersMsg20 = pythonIdentifiersMsg v1 s20
        let pythonAstMsgS21 = pythonAstMsg v1 s21

        -- Arrival of completion pm of s20 should generate no requests, because nothing depends on it
        B.newProduct pythonIdentifiersMsg20 `shouldBeAsSetTuple`
          ([], [])

        -- Arrival of ast pm of s21 should generate identifer request for s21
        B.newProduct pythonAstMsgS21 `shouldBeAsSetTuple`
          ([Request s21 pythonIdentifierFinder [Req.ProductMessage pythonAstMsgS21]], [])

        -- After registration of a dynamic dependency, that is already fulfilled, the request for it should be generated immediately
        -- completions of s21 now depends on ast of s21 (via product dependency) and on completions of s20 (via dynamic dependency)

        -- Registration of dynamic dependency 'code completions service for s21 depends on completions product of s20' should
        -- immediately generate request for itself, because all products and sources are already available
        B.newDynamicDependency (RegisterDynamicDependencies s21 pythonIdentifierFinder [DynamicDependency s20 pythonIdentifierFinder identifiers python]) `shouldBe'`
          Just (Request s21 pythonIdentifierFinder [Req.ProductMessage pythonAstMsgS21, Req.ProductMessage pythonIdentifiersMsg20])

        -- Right now the IDE sends a CommandMessage to the pythonCodeCompletion service, because the user wants completions at one specific region in "s20"
        -- The broker doesn't get notified about any of this (by design).
        -- The pythonCodeCompletion service then noticies, that it needs the identifiers for "s20" to compare them to the text in the selected region.
        -- So it declares a CommandMessage dependency.

        -- This dependency is already fulfilled, so the CommandMessage should be returned immediately
        B.newCommandMessageDependency (RegisterCommandMessageDependencies
                                         (CommandMessage 1 1 "completionsForRegion" python "77:5" [])
                                         [DynamicDependency s20 pythonIdentifierFinder identifiers python]) `shouldBe'`
          Just (CommandMessage 1 1 "completionsForRegion" python "77:5" [Req.ProductMessage pythonIdentifiersMsg20])

        -- Then the user wants completions for "s21".
        -- The identifiers for "s21" have not yet arrived, so no CommandMessage should be returned
        B.newCommandMessageDependency (RegisterCommandMessageDependencies
                                         (CommandMessage 1 1 "completionsForRegion" python "5:0" [])
                                         [DynamicDependency s21 pythonIdentifierFinder identifiers python]) `shouldBe'`
          Nothing

        -- Now the completions for "s21" arrive and should trigger the resend of the "s21" pythonCodeCompletion CommandMessage
        let pythonIdentifiersMsg21 = pythonIdentifiersMsg v1 s21
        B.newProduct pythonIdentifiersMsg21 `shouldBeAsSetTuple`
          ([],[CommandMessage 1 1 "completionsForRegion" python "5:0" [Req.ProductMessage pythonIdentifiersMsg21]])

        --
        --     s1                s2                s3
        --     ^                 ^                 ^
        --     |                 |                 |
        --    PRO               PRO               PRO
        --     |                 |                 |
        --    ast               ast               ast
        --     ^                 ^                 ^
        --     |                 |                 |
        --    PRO               PRO               PRO
        --     |                 |                 |
        --   errors <-- DYN -- errors <-- DYN -- errors
        --
        let ast1s1 = javaAstMsg v1 s1
            ast1s2 = javaAstMsg v1 s2
            ast1s3 = javaAstMsg v1 s3
            type1s1 = javaTypeMsg v1 s1
            type1s2 = javaTypeMsg v1 s2
            type1s3 = javaTypeMsg v1 s3

        -- Registration of dynamic dependency 'java typechecker service for s3 depends on errors of s2' should not generate requests
        B.newDynamicDependency (RegisterDynamicDependencies s3 javaTypechecker [DynamicDependency s2 javaTypechecker errors java]) `shouldBe'`
          Nothing

        -- Arrival of sm of s1 should generate parser request for s1
        B.newVersion (javaS1 v1) `shouldBeAsSetTuple`
          ([Request s1 javaParser [Req.SourceMessage (javaS1 v1)]], [])

        -- Arrival of ast pm of s1 should generate typechecker request for s1
        B.newProduct ast1s1 `shouldBeAsSetTuple`
          ([Request s1 javaTypechecker [Req.ProductMessage ast1s1, Req.SourceMessage (javaS1 v1)]], [])

        -- Arrival of sm of s2 should generate parser request for s2
        B.newVersion (javaS2 v1) `shouldBeAsSetTuple`
          ([Request s2 javaParser [Req.SourceMessage (javaS2 v1)]], [])

        -- Arrival of ast pm of s2 should generate typechecker request for s2
        -- note: there is no dyn dep on errors of s2 yet, only on errors of s3
        B.newProduct ast1s2 `shouldBeAsSetTuple`
          ([Request s2 javaTypechecker [Req.ProductMessage ast1s2, Req.SourceMessage (javaS2 v1)]], [])

        -- Registration of dynamic dependency 'java typechecker service for s2 depends on errors of s1' should not generate requests
        B.newDynamicDependency (RegisterDynamicDependencies s2 javaTypechecker [DynamicDependency s1 javaTypechecker errors java]) `shouldBe'`
          Nothing

        -- Arrival of errors pm of s1 should generate typechecker request for s2, because of dynamic dependency
        B.newProduct type1s1 `shouldBeAsSetTuple`
          ([Request s2 javaTypechecker [Req.ProductMessage type1s1, Req.ProductMessage ast1s2, Req.SourceMessage(javaS2 v1)]], [])

        -- Arrival of errors pm of s2 should generate no requests, because product dependencies of errors of s3 are not satiesfied
        B.newProduct type1s2 `shouldBeAsSetTuple`
          ([], [])

        -- Arrival of sm of s3 should generate parser request for s3
        B.newVersion (javaS3 v1) `shouldBeAsSetTuple`
          ([Request s3 javaParser [Req.SourceMessage (javaS3 v1)]], [])

        -- Arrival of ast pm of s3 should generate typechecker request for s3
        B.newProduct ast1s3 `shouldBeAsSetTuple`
          ([Request s3 javaTypechecker [Req.ProductMessage type1s2, Req.ProductMessage ast1s3, Req.SourceMessage (javaS3 v1)]], [])

        -- Arrival of errors pm of s1 should still generate typechecker request for s2, because of dynamic dependency
        B.newProduct type1s1 `shouldBeAsSetTuple`
          ([Request s2 javaTypechecker [Req.ProductMessage ast1s2, Req.ProductMessage type1s1, Req.SourceMessage(javaS2 v1)]], [])

        -- Arrival of errors pm of s3 should generate no request, because nothing depends on it
        B.newProduct type1s3 `shouldBeAsSetTuple`
          ([], [])


  where
    shouldBe' :: (Eq a, Show a, MonadTrans t, MonadState s (t IO)) =>
      (s -> (a, s)) -> a -> t IO ()
    shouldBe' actual expected = do
      act <- state actual
      lift $ act `shouldBe` expected

    shouldBeAsSetTuple :: (Ord a1, Ord a2, Show a1, Show a2, MonadTrans t, MonadState s (t IO)) =>
      (s -> (([a1], [a2]), s)) -> ([a1], [a2]) -> t IO ()
    shouldBeAsSetTuple actual (expectedRequests,expectedCommandMessages) = do
      act <- state actual
      lift $ case act of
        (actualRequests,actualCommandMessages) ->
          (S.fromList actualRequests, S.fromList actualCommandMessages) `shouldBe` (S.fromList expectedRequests, S.fromList expectedCommandMessages)

