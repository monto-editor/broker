{-# LANGUAGE CPP, OverloadedStrings #-}
module BrokerSpec(spec) where

import           Control.Monad.State
import           Control.Arrow

import qualified Data.Set as S
import           Data.Aeson (toJSON)

import           Debug.Trace

import qualified Monto.Broker as B
import qualified Monto.SourceMessage as S
import qualified Monto.ProductMessage as P
import qualified Monto.ProductDescription as PD
import           Monto.ProductDependency as PDEP
import           Monto.Types
import           Monto.RegisterServiceRequest
import           Monto.Request

import           Test.Hspec


spec :: Spec
spec = do

  let register sid prods deps = B.registerService (RegisterServiceRequest sid "" "" Nothing prods deps)
      java = "java" :: Language
      tokens = "tokens" :: Product
      ast = "ast" :: Product
      completions = "completions" :: Product
      errors = "errors" :: Product
      javaTypechecker = "javaTypechecker" :: ServiceID
      javaCodeCompletion = "javaCodeCompletion" :: ServiceID
      javaParser = "javaParser" :: ServiceID
      javaTokenizer = "javaTokenizer" :: ServiceID
      javaSource = PDEP.ProductDependency "source" "source" java
      s1 i = S.SourceMessage i "s1" "java" ""
      s2 i = S.SourceMessage i "s2" "java" ""
      s3 i = S.SourceMessage i "s3" "java" ""
      astMsg vid src = P.ProductMessage vid src javaParser ast java "" (toJSON (0::Int))
      typMsg vid src = P.ProductMessage vid src javaTypechecker errors java "" (toJSON (0::Int))
      comMsg vid src = P.ProductMessage vid src javaCodeCompletion completions java "" (toJSON (0::Int))
      tokMsg vid src = P.ProductMessage vid src javaTokenizer tokens java "" (toJSON (0::Int))
      v1 = VersionID 1
      newVersion' v b = first S.fromList $ B.newVersion v b
      newProduct' v b = first S.fromList $ B.newProduct v b
      python = "python" :: Language
      productA = "productA" :: Product
      productB = "productB" :: Product
      s20 i = S.SourceMessage i "s20" "python" ""
      s21 i = S.SourceMessage i "s21" "python" ""
      serviceA = "serviceA" :: ServiceID
      serviceB = "serviceB" :: ServiceID
      pythonSource = PDEP.ProductDependency "source" "source" python
      productAMsg vid src = P.ProductMessage vid src serviceA productA python "" (toJSON (0::Int))

  context  "Static Dependencies" $ do

    let broker = register javaCodeCompletion [PD.ProductDescription completions java] [javaSource, ProductDependency javaParser ast java]
               $ register javaTypechecker [PD.ProductDescription errors java] [javaSource, ProductDependency javaParser ast java]
               $ register javaParser [PD.ProductDescription ast java] [javaSource]
               $ register javaTokenizer [PD.ProductDescription tokens java] [javaSource]
               $ B.empty (Port 5010) (Port 5020)

    it "can manage static server dependencies" $
      void $ flip execStateT broker $ do

        newVersion' (s1 v1) `shouldBe'` S.fromList
           [ Request "s1" javaParser [SourceMessage (s1 v1)]
           , Request "s1" javaTokenizer [SourceMessage (s1 v1)]
           ]

        let ast1 = astMsg v1 "s1"
        newProduct' ast1 `shouldBe'` S.fromList
           [ Request "s1" javaCodeCompletion [SourceMessage (s1 v1), ProductMessage ast1]
           , Request "s1" javaTypechecker [SourceMessage (s1 v1), ProductMessage ast1]
           ]

        newProduct' (tokMsg v1 "s1") `shouldBe'` S.fromList []
        newProduct' (comMsg v1 "s1") `shouldBe'` S.fromList []
        newProduct' (typMsg v1 "s1") `shouldBe'` S.fromList []

    it "can deal with outdated products" $ do
      _ <- flip execStateT broker $ do
        let currentId = VersionID 42
            outdatedId = VersionID 41

        newVersion' (s1 currentId) `shouldBe'` S.fromList
           [ Request "s1" javaParser [SourceMessage (s1 currentId)]
           , Request "s1" javaTokenizer [SourceMessage (s1 currentId)]
           ]

        -- outdated product arrives
        newProduct' (astMsg outdatedId "s1") `shouldBe'` S.fromList []

      return ()


  context "Product Dependencies" $ do

    let broker = register serviceB [PD.ProductDescription productB python] [PDEP.ProductDependency serviceA productA python]
               $ register serviceA [PD.ProductDescription productA python] [pythonSource]
               $ register javaTypechecker [PD.ProductDescription errors java] [javaSource,PDEP.ProductDependency javaParser ast java]
               $ register javaParser [PD.ProductDescription ast java] [javaSource]
               $ B.empty (Port 5010) (Port 5020)

    it "can track dynamic product dependencies" $
      --
      --     s1      s2      s3
      --     ^       ^       ^
      --    ast     ast     ast
      --     ^       ^       ^
      --   type <- type <- type
      --
      --
      --    s20      s21
      --     ^        ^
      --   prodA <- prodA
      --     ^        ^
      --   prodB    prodB
      --
      void $ flip execStateT broker $ do

        let serviceAs20 = productAMsg v1 "s20"

        trace "s20" $ B.newVersion (s20 v1) `shouldBe'`
          [Request "s20" serviceA [SourceMessage (s20 v1)]]

        trace "s21" $ B.newVersion (s21 v1) `shouldBe'`
          [Request "s21" serviceA [SourceMessage (s21 v1)]]

        modify $ B.registerDynamicDependency "s21" serviceA [([(productA, python)], ("s20", serviceA))]

        trace "s21" $ B.newVersion (s21 v1) `shouldBe'`
          []

        trace "service + prod dep test" $ B.newProduct serviceAs20 `shouldBe'`
          [Request "s20" serviceB [ProductMessage serviceAs20], Request "s21" serviceA [ProductMessage serviceAs20, SourceMessage (s21 v1)]]

        let ast1s1 = astMsg v1 "s1"
            ast1s2 = astMsg v1 "s2"
            ast1s3 = astMsg v1 "s3"
            typ1s1 = typMsg v1 "s1"
            typ1s2 = typMsg v1 "s2"
            typ1s3 = typMsg v1 "s3"

        modify $ B.registerDynamicDependency "s3" javaTypechecker [([(errors, java)], ("s2",javaTypechecker))]

        trace "s1" $ B.newVersion (s1 v1) `shouldBe'`
          [Request "s1" javaParser [SourceMessage (s1 v1)]]

        trace "a1" $ B.newProduct ast1s1 `shouldBe'`
          [Request "s1" javaTypechecker [ProductMessage ast1s1, SourceMessage (s1 v1)]]

        trace "s2" $ B.newVersion (s2 v1) `shouldBe'`
          [Request "s2" javaParser [SourceMessage (s2 v1)]]

        trace "a2" $ B.newProduct ast1s2 `shouldBe'`
          [Request "s2" javaTypechecker [ProductMessage ast1s2, SourceMessage (s2 v1)]]

        modify $ B.registerDynamicDependency "s2" javaTypechecker [([(errors, java)], ("s1",javaTypechecker))]

        trace "t1" $ B.newProduct typ1s1 `shouldBe'`
          [Request "s2" javaTypechecker [ProductMessage typ1s1, ProductMessage ast1s2, SourceMessage(s2 v1)]]

        trace "t2" $ B.newProduct typ1s2 `shouldBe'`
          []

        trace "s3" $ B.newVersion (s3 v1) `shouldBe'`
          [Request "s3" javaParser [SourceMessage (s3 v1)]]

        trace "a3" $ B.newProduct ast1s3 `shouldBe'`
          [Request "s3" javaTypechecker [ProductMessage typ1s2, ProductMessage ast1s3, SourceMessage (s3 v1)]]

        trace "t1" $ B.newProduct typ1s1 `shouldBe'`
          [Request "s2" javaTypechecker [ProductMessage ast1s2, ProductMessage typ1s1, SourceMessage(s2 v1)]]

        trace "t3" $ B.newProduct typ1s3 `shouldBe'`
          []

    -- A
    -- ^
    -- B < D    D3   A1
    -- ^   ^    ^    ^
    -- |  /     B2   B3
    -- C         ^    ^
    -- ^          \  /
    -- E           C1
    --

  where
    shouldBe' actual expected = do
      act <- state actual
      lift $ act `shouldBe` expected
