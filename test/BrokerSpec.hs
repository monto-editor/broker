{-# LANGUAGE CPP, OverloadedStrings #-}
module BrokerSpec(spec) where

import           Control.Monad.State

import qualified Data.Vector as V
import qualified Data.Map as M

import           Monto.Broker (Response(..))
import qualified Monto.Broker as B
import qualified Monto.VersionMessage as V
import qualified Monto.ProductMessage as P
import           Monto.ProductDependency
import           Monto.Types
import           Monto.RegisterServiceRequest
import           Monto.ServiceDependency

import           Test.Hspec


spec :: Spec
spec = do

  let register sid lang prod deps = B.registerService (RegisterServiceRequest sid "" "" lang prod Nothing (V.fromList deps))
      java = "java" :: Language
      tokens = "tokens" :: Product
      ast = "ast" :: Product
      completions = "completions" :: Product
      errors = "errors" :: Product
      javaTypechecker = "javaTypechecker" :: ServiceID
      javaCodeCompletion = "javaCodeCompletion" :: ServiceID
      javaParser = "javaParser" :: ServiceID
      javaTokenizer = "javaTokenizer" :: ServiceID
      javaSource = SourceDependency java
      s1 i = V.VersionMessage i "s1" "java" "" Nothing
      s2 i = V.VersionMessage i "s2" "java" "" Nothing
      astMsg vid src deps = P.ProductMessage vid src javaParser ast java "" (V.fromList deps)
      typMsg vid src deps = P.ProductMessage vid src javaTypechecker errors java "" (V.fromList deps)
      comMsg vid src deps = P.ProductMessage vid src javaCodeCompletion completions java "" (V.fromList deps)
      tokMsg vid src deps = P.ProductMessage vid src javaTokenizer tokens java "" (V.fromList deps)
      v1 = VersionID 1

  context  "Static Dependencies" $ do

    let broker = register javaCodeCompletion java completions [javaSource, ServiceDependency javaParser]
               $ register javaTypechecker java errors [javaSource, ServiceDependency javaParser]
               $ register javaParser java ast [javaSource]
               $ register javaTokenizer java tokens [javaSource]
               $ B.empty (Port 5010) (Port 5020)
        javaTokenizerService = B.services broker M.! javaTokenizer
        javaParserService = B.services broker M.! javaParser
        javaCodeCompletionService = B.services broker M.! javaCodeCompletion
        javaTypecheckerService = B.services broker M.! javaTypechecker

    it "can manage static server dependencies" $
      void $ flip execStateT broker $ do

        B.newVersion (s1 v1) `shouldSatisfy'` \responses ->
              length responses == 2
           && Response "s1" javaParserService [B.VersionMessage (s1 v1)] `elem` responses
           && Response "s1" javaTokenizerService [B.VersionMessage (s1 v1)] `elem` responses

        let ast1 = astMsg v1 "s1" []
        B.newProduct ast1 `shouldSatisfy'` \responses ->
              length responses == 2
           && Response "s1" javaCodeCompletionService [B.ProductMessage ast1] `elem` responses
           && Response "s1" javaTypecheckerService [B.ProductMessage ast1] `elem` responses

        B.newProduct (tokMsg v1 "s1" []) `shouldBe'` []
        B.newProduct (comMsg v1 "s1" []) `shouldBe'` []
        B.newProduct (typMsg v1 "s1" []) `shouldBe'` []

    it "can deal with outdated products" $ do
      _ <- flip execStateT broker $ do
        let currentId = VersionID 42
            outdatedId = VersionID 41
        B.newVersion (s1 currentId) `shouldSatisfy'` \responses ->
              length responses == 2
           && Response "s1" javaParserService [B.VersionMessage (s1 currentId)] `elem` responses
           && Response "s1" javaTokenizerService [B.VersionMessage (s1 currentId)] `elem` responses

        -- outdated product arrives
        B.newProduct (astMsg outdatedId "s1" []) `shouldBe'` []

      return ()


  context "Product Dependencies" $ do

    let broker = register javaTypechecker java errors [javaSource,ServiceDependency javaParser]
               $ register javaParser java ast [javaSource]
               $ B.empty (Port 5010) (Port 5020)
        javaParserService = B.services broker M.! javaParser
        javaTypecheckerService = B.services broker M.! javaTypechecker

    it "can track dynamic product dependencies" $
      --
      --     s1      s2      s3
      --     ^       ^       ^
      --    ast     ast     ast
      --     ^       ^       ^
      --   type <- type <- type
      --
      void $ flip execStateT broker $ do
        B.registerProductDependency
             (ProductDependency v1 "s2" javaTypechecker)
             [ProductDependency v1 "s1" javaTypechecker] `shouldBe'`
          ["s1"]

        B.registerProductDependency
             (ProductDependency v1 "s3" javaTypechecker)
             [ProductDependency v1 "s2" javaTypechecker] `shouldBe'`
          ["s2"]

        B.newVersion (s1 v1) `shouldBe'`
          [Response "s1" javaParserService [B.VersionMessage (s1 v1)]]

        let ast1s1 = astMsg v1 "s1" []
            ast1s2 = astMsg v1 "s2" []
            typ1s1 = typMsg v1 "s1" []

        B.newProduct ast1s1 `shouldBe'`
          [Response "s1" javaTypecheckerService [B.ProductMessage ast1s1]]

        B.newVersion (s2 v1) `shouldBe'`
          [Response "s2" javaParserService [B.VersionMessage (s2 v1)]]

        B.newProduct ast1s2 `shouldBe'` []

        B.newProduct typ1s1 `shouldBe'`
          [Response "s2" javaTypecheckerService [B.ProductMessage ast1s2,B.ProductMessage typ1s1]]

  where
    shouldBe' actual expected = do
      act <- state actual
      lift $ act `shouldBe` expected


    shouldSatisfy' actual expected = do
      act <- state actual
      lift $ act `shouldSatisfy` expected
