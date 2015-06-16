{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module BrokerSpec(spec) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Monad (void)
#endif
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
      void $ flip execStateT broker $ do
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
      void $ flip execStateT broker $ do
        B.registerProductDep (Product ("s2","json","type")) [Product ("s1","json","type")] `shouldBe'`
          ["s1"]

        B.registerProductDep (Product ("s3","json","type")) [Product ("s2","json","type")] `shouldBe'`
          ["s2"]

        B.newVersion (s1 1) `shouldBe'`
          [Response "s1" ast [B.VersionMessage (s1 1)]]

        B.newProduct (astMsg 1 1 "s1") `shouldBe'`
          [Response "s1" typ [B.ProductMessage (astMsg 1 1 "s1")]]

        B.newVersion (s2 1) `shouldBe'`
          [Response "s2" ast [B.VersionMessage (s2 1)]]

        B.newProduct (astMsg 1 1 "s2") `shouldBe'` []

        B.newProduct (typMsg 1 1 "s1") `shouldBe'`
          [Response "s2" typ [B.ProductMessage (astMsg 1 1 "s2"),B.ProductMessage (typMsg 1 1 "s1")]]

  where
    shouldBe' actual expected = do
      act <- state actual
      lift $ act `shouldBe` expected
