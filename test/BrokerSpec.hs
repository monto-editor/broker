{-# LANGUAGE OverloadedStrings #-}
module BrokerSpec(spec) where

import           Control.Exception (throw,catch,SomeException)
import           Control.Monad.State

import qualified Data.Text.IO as TIO

import           Monto.Broker (Response(..))
import qualified Monto.Broker as B
import           Monto.DependencyManager (Server(..),Dependency(..),ProductDependency(..))
import qualified Monto.VersionMessage as V
import qualified Monto.ProductMessage as P

import           Test.Hspec

spec :: Spec
spec = do

  let typ = Server "type" "json"
      ast = Server "ast" "json"
      broker = B.register typ [Dependency ast]
             $ B.register ast [Bottom]
             $ B.empty

  it "can track dynamic product dependencies" $ do
    --
    --     s1      s2      s3
    --     ^       ^       ^
    --    ast     ast     ast
    --     ^       ^       ^
    --   type <- type <- type 
    --
    let s1 i = V.VersionMessage i "s1" "java" "" Nothing
        s2 i = V.VersionMessage i "s2" "java" "" Nothing
        astMsg vid pid src = P.ProductMessage vid pid src "ast" "json" "" Nothing
        typMsg vid pid src = P.ProductMessage vid pid src "type" "json" "" Nothing

    broker' <- flip execStateT broker $ do
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
    execute test = do
      brker <- get
      lift $ (test `catch` \e -> do
        print brker
        TIO.writeFile "dynamic.dot" (B.dynamicDependencyManagerToDot brker)
        TIO.writeFile "static.dot" (B.staticDependencyManagerToDot brker)
        throw (e :: SomeException)
        )

    shouldBe' actual expected = do
      act <- state actual
      execute $ act `shouldBe` expected
