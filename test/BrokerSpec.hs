{-# LANGUAGE OverloadedStrings #-}
module BrokerSpec(spec) where

import           Control.Monad.State

import           Monto.Broker (Response(..),Server(..),Dependency(..),ProductDependency(..))
import qualified Monto.Broker as B
import qualified Monto.VersionMessage as V
import qualified Monto.ProductMessage as P

import           Test.Hspec

spec :: Spec
spec = do

  let typ = Server "type" "json"
      com = Server "completion" "json"
      ast = Server "ast" "json"
      syn = Server "syntax" "json"
      s1 i = V.VersionMessage i "s1" "java" "" Nothing
      s2 i = V.VersionMessage i "s2" "java" "" Nothing
      astMsg vid pid src = P.ProductMessage vid pid src "ast" "json" "" Nothing
      typMsg vid pid src = P.ProductMessage vid pid src "type" "json" "" Nothing
      comMsg vid pid src = P.ProductMessage vid pid src "completion" "json" "" Nothing
      synMsg vid pid src = P.ProductMessage vid pid src "syntax" "json" "" Nothing

  context "Static Dependencies" $ do
    let broker = B.registerServer com [Dependency ast]
               $ B.registerServer typ [Dependency ast]
               $ B.registerServer ast [Bottom]
               $ B.registerServer syn [Bottom]
               $ B.empty

    it "can manage static server dependencies" $ do
      _ <- flip execStateT broker $ do
        B.newVersion (s1 1) `shouldBe'`
          [ Response "s1" ast [B.VersionMessage (s1 1)]
          , Response "s1" syn [B.VersionMessage (s1 1)]
          ]
        
        B.newProduct (astMsg 1 1 "s1") `shouldBe'`
          [ Response "s1" com [B.ProductMessage (astMsg 1 1 "s1")]
          , Response "s1" typ [B.ProductMessage (astMsg 1 1 "s1")]
          ]
        
        B.newProduct (synMsg 1 1 "s1") `shouldBe'` []
        B.newProduct (comMsg 1 1 "s1") `shouldBe'` []
        B.newProduct (typMsg 1 1 "s1") `shouldBe'` []

      return ()

    it "can deal with outdated products" $ do
      _ <- flip execStateT broker $ do
        let currentId = 42
            outdatedId = 41
        B.newVersion (s1 currentId) `shouldBe'`
          [ Response "s1" ast [B.VersionMessage (s1 currentId)]
          , Response "s1" syn [B.VersionMessage (s1 currentId)]
          ]

        -- outdated product arrives
        B.newProduct (astMsg outdatedId 1 "s1") `shouldBe'` []

      return ()
      

  context "Product Dependencies" $ do

    let broker = B.registerServer typ [Dependency ast]
               $ B.registerServer ast [Bottom]
               $ B.empty

    it "can track dynamic product dependencies" $ do
      --
      --     s1      s2      s3
      --     ^       ^       ^
      --    ast     ast     ast
      --     ^       ^       ^
      --   type <- type <- type 
      --
      _ <- flip execStateT broker $ do
        B.registerProductDep (Product ("s2","type","json")) [Product ("s1","type","json"),Product ("s2","ast","json")] `shouldBe'`
          ["s1","s2"]

        B.registerProductDep (Product ("s3","type","json")) [Product ("s2","type","json"),Product ("s3","ast","json")] `shouldBe'`
          ["s2","s3"]

        B.newVersion (s1 1) `shouldBe'`
          [Response "s1" ast [B.VersionMessage (s1 1)]]

        B.newProduct (astMsg 1 1 "s1") `shouldBe'`
          [Response "s1" typ [B.ProductMessage (astMsg 1 1 "s1")]]

        B.newVersion (s2 1) `shouldBe'`
          [Response "s2" ast [B.VersionMessage (s2 1)]]

        B.newProduct (astMsg 1 1 "s2") `shouldBe'`
          [Response "s2" typ [B.ProductMessage (astMsg 1 1 "s2")]]

        B.newProduct (typMsg 1 1 "s1") `shouldBe'`
          [Response "s2" typ [B.ProductMessage (astMsg 1 1 "s2"),B.ProductMessage (typMsg 1 1 "s1")]]

      return ()

  where
    shouldBe' actual expected = do
      act <- state actual
      lift $ act `shouldBe` expected
