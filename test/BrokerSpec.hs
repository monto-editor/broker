{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections #-}
module BrokerSpec where

import           Prelude hiding (product)
import           Control.Applicative

import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Graph.Inductive as G

import           Monto.Dependency
import           Monto.VersionMessage (VersionMessage)
import qualified Monto.VersionMessage as V
import           Monto.ProductMessage (ProductMessage)
import qualified Monto.ProductMessage as P
import           Monto.MessageStore (MessageStore)
import qualified Monto.MessageStore as Store

import           Test.Hspec
import           Test.QuickCheck hiding (vectorOf)
import qualified Test.QuickCheck as Q

liftT3 :: Applicative f => f a -> f b -> f c -> f (a,b,c)
liftT3 fa fb fc = (\a b c -> (a,b,c)) <$> fa <*> fb <*> fc

liftT5 :: Applicative f => f a -> f b -> f c -> f d -> f e -> f (a,b,c,d,e)
liftT5 fa fb fc fd fe = (\a b c d e -> (a,b,c,d,e)) <$> fa <*> fb <*> fc <*> fd <*> fe

instance Arbitrary Dependency where
  arbitrary = oneof
    [ Version <$> liftT3 arbitraryPositive arbitrarySource arbitraryLanguage
    , Product <$> liftT5 arbitraryPositive arbitraryPositive arbitrarySource arbitraryLanguage arbitraryProduct
    ]

instance Arbitrary VersionMessage where
  arbitrary =
    V.VersionMessage
      <$> arbitraryPositive
      <*> arbitrarySource
      <*> arbitraryLanguage
      <*> return T.empty
      <*> return Nothing
      <*> return Nothing

vectorOf :: Gen a -> Gen (Vector a)
vectorOf g = V.fromList <$> Q.listOf g

arbitraryPositive :: Gen Int
arbitraryPositive = getPositive <$> arbitrary

arbitrarySource :: Gen Source
arbitrarySource = do
  source <- T.pack . show <$> arbitraryPositive
  return $ T.append "S" source

arbitraryProduct :: Gen Product
arbitraryProduct = do
  product <- T.pack . show <$> arbitraryPositive
  return $ T.append "P" product

arbitraryLanguage :: Gen Language
arbitraryLanguage = do
  language <- T.pack . show <$> arbitraryPositive
  return $ T.append "L" language

instance Arbitrary ProductMessage where
  arbitrary =
    P.ProductMessage
      <$> arbitraryPositive
      <*> arbitraryPositive
      <*> arbitrarySource
      <*> arbitraryProduct
      <*> arbitraryLanguage
      <*> return T.empty
      <*> (Just <$> vectorOf arbitrary)
      <*> return Nothing

{-arbitraryProductAndDependency :: [Dependency] -> Gen (ProductMessage,[Dependency])-}
{-arbitraryProductAndDependency versions = do-}
  {-product <- arbitrary-}
  {-dependencies <- nonEmptySubset versions-}
  {-return (product,dependencies)-}

{-fillStoreWithProducts :: MessageStore -> Gen MessageStore-}
{-fillStoreWithProducts store-}
  {-| M.null (Store.versions store) = return store-}
  {-| otherwise = do-}
      {-n <- arbitrarySizedIntegral-}
      {-iterateM (abs n) go store-}
  {-where-}
    {-go s = do-}
      {-(product,deps) <- arbitraryProductAndDependency (map Version (M.keys (Store.versions s)) ++ map Product (M.keys (Store.products s)))-}
      {-return $ addProductAndDependencies product deps s-}

{-addProductAndDependencies :: ProductMessage -> [Dependency] -> MessageStore -> MessageStore-}
{-addProductAndDependencies product deps store = store-}
  {-{ Store.products = M.insert (productId product) product (Store.products store)-}
  {-, Store.dependencies = G.insMapEdges nm' edges dep'-}
  {-, Store.nodeMap = nm'-}
  {-}-}
  {-where-}
    {-pd = Product (productId product)-}
    {-edges = [ (d,pd,()) | d <- deps ]-}
    
    {-(dep',nm',_) = G.insMapNode (Store.nodeMap store) pd (Store.dependencies store) -}

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 _ a = return a
iterateM n f a = f a >>= iterateM (n-1) f

instance Arbitrary MessageStore where
  arbitrary = do
    n <- arbitraryPositive
    iterateM n fillStore Store.empty

fillStore :: MessageStore -> Gen MessageStore
fillStore store =
  oneof
    [ snd <$> (Store.updateVersion  <$> arbitrary <*> pure store)
    , snd <$> (Store.updateProduct' <$> arbitrary <*> pure store)
    , snd <$> (Store.updateProduct' <$> deriveArbitraryProduct store <*> pure store)
    ]

deriveArbitraryProduct :: MessageStore -> Gen ProductMessage
deriveArbitraryProduct store = do
  if M.size (Store.versions store) == 0
    then arbitrary
    else do
      vn <- elements $ M.elems (Store.versions store)
      let Just (Version (vid,s,_)) = G.lab (Store.dependencies store) vn
      deps <- nonEmptySubset $ map snd $ G.labNodes (Store.dependencies store)
      pid  <- arbitraryPositive
      p    <- arbitraryProduct
      l    <- arbitraryLanguage
      return $ P.ProductMessage vid pid s p l "" (Just (V.fromList deps)) Nothing

nonEmptySubset :: [a] -> Gen [a]
nonEmptySubset [] = return []
nonEmptySubset xs = do
  i <- choose (0,length xs-1)
  dropArbitraryElements i xs

dropArbitraryElements :: Int -> [a] -> Gen [a]
dropArbitraryElements n xs
  | n > length xs = return []
  | otherwise     = go n (length xs) xs
  where
    go 0 _ ys = return ys
    go i l ys = do
      j <- choose (0,l-1)
      let (hs,ts) = L.splitAt j ys
      go (i-1) (l-1) $ hs ++ tail ts

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Message Broker" $ do

  it "ignores products with no dependencies" $
    property $ \(m :: MessageStore) ->
      let deps = Store.dependencies m
          productHasDependencies (node,Product _) = G.outdeg deps node > 0
          productHasDependencies _                = True
      in conjoin $ productHasDependencies <$> G.labNodes deps

  it "doesn't allow dependencies from versions to versions" $
    property $ \(m :: MessageStore) ->
      let deps = Store.dependencies m
          noVersionToVersionDep (from,to) =
            case (G.lab deps from,G.lab deps to) of
              (Just v@(Version _), Just u@(Version _)) ->
                counterexample ("there is a dependency between " ++ show v ++ " and " ++ show u) False
              _ -> property True
      in conjoin $ noVersionToVersionDep <$> G.edges deps
