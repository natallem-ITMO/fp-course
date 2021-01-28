{-# LANGUAGE LambdaCase #-}

module Block3.Task1Spec
  ( 
    spec
  ) where

import Block3.Task1 (eitherConcat, maybeConcat)

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "maybeConcat" $ do
    it "only Nothing" $ do
      (maybeConcat [Nothing] :: [Int]) `shouldBe` []
      (maybeConcat [Nothing, Nothing] :: [Double]) `shouldBe` []
      (maybeConcat [Nothing, Nothing, Nothing, Nothing, Nothing] :: [Char])
        `shouldBe` []
    it "only Just" $ do
      maybeConcat [Just [4 :: Int]] `shouldBe` [4]
      maybeConcat [Just [4 :: Int], Just [3, 4, 1]] `shouldBe` [4, 3, 4, 1]
      maybeConcat [Just [4 :: Int], Just [3, 4, 1], Just [0]] `shouldBe` [4, 3, 4, 1, 0]
      maybeConcat [Just [4 :: Int, 2, 6, 7], Just [3, 4, 1], Just [0]] `shouldBe` [4, 2, 6, 7, 3, 4, 1, 0]
    it "Just and Nothing" $ do
      maybeConcat [Nothing, Just [4 :: Int]] `shouldBe` [4]
      maybeConcat [Just [4 :: Int], Nothing] `shouldBe` [4]
      maybeConcat [Nothing, Just [4 :: Int], Nothing, Nothing]
        `shouldBe` [4]
      maybeConcat [Nothing, Just [4 :: Int], Just [3, 4, 1]]
        `shouldBe` [4, 3, 4, 1]
      maybeConcat [Just [4 :: Int], Nothing, Just [3, 4, 1]]
        `shouldBe` [4, 3, 4, 1]
      maybeConcat [Nothing, Just [4 :: Int], Nothing, Just [3, 4, 1], Nothing] `shouldBe` [4, 3, 4, 1]
      maybeConcat [Just [4 :: Int], Just [3, 4, 1], Nothing]
        `shouldBe` [4, 3, 4, 1]
      maybeConcat [Just [4 :: Int], Nothing, Nothing, Nothing, Just [3, 4, 1], Nothing]
        `shouldBe` [4, 3, 4, 1]
      maybeConcat [Nothing, Just [4 :: Int], Just [3, 4, 1], Nothing, Nothing, Nothing, Just [0]]
        `shouldBe` [4, 3, 4, 1, 0]
      maybeConcat
        [Just [4 :: Int, 2, 6, 7], Nothing, Nothing, Just [3, 4, 1], Just [0], Nothing, Nothing]
        `shouldBe` [4, 2, 6, 7, 3, 4, 1, 0]
  describe "eitherConcat" $ do
    it "either Sum" $ do
      eitherConcat
        [Left (Sum (0 :: Int)), Right ([] :: [Char])]
        `shouldBe` (Sum {getSum = 0}, [])
      eitherConcat
        [Left (Sum (0 :: Int)), Right ("Hello, "), Left (Sum 4), Right ("my friend!")]
        `shouldBe` (Sum {getSum = 4}, "Hello, my friend!")
      eitherConcat
        [Left (Sum (3 :: Int)), Right [1, (3 :: Int)]]
        `shouldBe` (Sum {getSum = 3}, [1, 3])
      eitherConcat
        [Left (Sum (3 :: Int)), Right [3, 4, 2, 3], Left (Sum 5), Right [1, (3 :: Int)]]
        `shouldBe` (Sum {getSum = 8}, [3, 4, 2, 3, 1, 3])
      eitherConcat
        [Left (Sum (3 :: Int)), Right [1, 2, (3 :: Int)], Left (Sum 5), Right [4, 5]]
        `shouldBe` (Sum {getSum = 8}, [1, 2, 3, 4, 5])
      eitherConcat
        [Left (Sum (3 :: Int)), Left (Sum 5), Right [1, 2, (3 :: Int)]]
        `shouldBe` (Sum {getSum = 8}, [1, 2, 3])
    it "either Product" $ do
      eitherConcat 
        [Right (Product 0), Left "It's ", Right (Product (4 :: Int)), Left "ZERO"]
        `shouldBe` ("It's ZERO", Product {getProduct = 0})
      eitherConcat 
        [Right (Product (5 :: Int)), Left "I'm ", Left "almost ", Right (Product 4), Left "done"]
        `shouldBe` ("I'm almost done", Product {getProduct = 20})
      eitherConcat 
        [Right (Product (-56 :: Int)), Left "I'm ", Right (Product (-1)), Right (Product (-1)), Left "done..."]
        `shouldBe` ("I'm done...", Product {getProduct = (-56)})

-- | newtype from lecture as monoid
newtype Sum a = Sum {getSum :: a}
  deriving (Show, Eq)

-- | newtype from lecture as monoid
newtype Product a = Product {getProduct :: a}
  deriving (Show, Eq)

instance Num a => Semigroup (Sum a) where
  Sum x <> Sum y = Sum (x + y)

instance Num a => Semigroup (Product a) where
  Product x <> Product y = Product (x * y)

instance Num a => Monoid (Sum a) where
  mempty = (Sum 0)

instance Num a => Monoid (Product a) where
  mempty = (Product 1)
