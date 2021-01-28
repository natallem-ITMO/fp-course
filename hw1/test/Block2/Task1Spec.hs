{-# LANGUAGE LambdaCase #-}

module Block2.Task1Spec
  ( 
    spec
  ) where

import Data.Foldable (toList)
import Data.List (sort)

import Block1.Task3 (fromList)
import Block1.Task3Spec (testLists, treeTestList)

import Control.Exception (ErrorCall (..), evaluate)
import Test.Hspec (Spec, describe, it, shouldBe, shouldThrow)

spec :: Spec
spec = do
  describe "implemented foldable operations" $ do
    it "foldMap (:[])" $ do
      foldMap (: []) (treeTestList !! 0) `shouldBe` []
      foldMap (: []) (treeTestList !! 1) `shouldBe` [5]
      foldMap (: []) (treeTestList !! 4) `shouldBe` [5, 6, 6]
      foldMap (: []) (treeTestList !! 13) 
        `shouldBe` [2, 2, 3, 3, 3, 5, 5, 6, 6, 6, 8, 10]
    it "foldr" $ do
      foldr (+) 0 (treeTestList !! 0) `shouldBe` 0
      foldr (+) 1 (treeTestList !! 0) `shouldBe` 1
      foldr (*) 1 (treeTestList !! 1) `shouldBe` 5
      foldr (*) 5 (treeTestList !! 2) `shouldBe` 125
      foldr (\a -> (++) (show a ++ " ~ ")) "R" (treeTestList !! 2)
        `shouldBe` "5 ~ 5 ~ R"
      foldr (\a -> (++) (show a ++ " ~ ")) "R" (treeTestList !! 8)
        `shouldBe` "3 ~ 3 ~ 3 ~ 5 ~ 6 ~ 6 ~ 6 ~ R"
  describe "requered low" $ do
    it "toList . fromList = sort" $ do
      (toList . fromList) (testLists !! 0) 
        `shouldBe` sort (testLists !! 0)
      (toList . fromList) (testLists !! 1) 
        `shouldBe` sort (testLists !! 1)
      (toList . fromList) (testLists !! 2) 
        `shouldBe` sort (testLists !! 2)
      (toList . fromList) (testLists !! 3) 
        `shouldBe` sort (testLists !! 3)
  describe "basic foldable operations" $ do
    it "foldl" $ do
      foldl (+) 0 (treeTestList !! 0) `shouldBe` 0
      foldl (+) 1 (treeTestList !! 0) `shouldBe` 1
      foldl (*) 1 (treeTestList !! 1) `shouldBe` 5
      foldl (*) 5 (treeTestList !! 2) `shouldBe` 125
      foldl (\s a -> (++) s (" ~ " ++ show a)) "L" (treeTestList !! 2)
        `shouldBe` "L ~ 5 ~ 5"
      foldl (\s a -> (++) s (" ~ " ++ show a)) "L" (treeTestList !! 8)
        `shouldBe` "L ~ 3 ~ 3 ~ 3 ~ 5 ~ 6 ~ 6 ~ 6"
    it "foldl1" $ do
      evaluate (foldl1 (+) (treeTestList !! 0))
        `shouldThrow`
        (
          \case
             ErrorCall "foldl1: empty structure" -> True
             _                                   -> False
        )
      foldl1 (*) (treeTestList !! 1) `shouldBe` 5
      foldl1 (*) (treeTestList !! 2) `shouldBe` 25
      foldl1 (*) (treeTestList !! 4) `shouldBe` 180
    it "foldr1" $ do
      evaluate (foldr1 (+) (treeTestList !! 0))
        `shouldThrow`
        (
          \case
             ErrorCall "foldr1: empty structure" -> True
             _                                   -> False
        )
      foldr1 (*) (treeTestList !! 1) `shouldBe` 5
      foldr1 (*) (treeTestList !! 2) `shouldBe` 25
      foldr1 (*) (treeTestList !! 4) `shouldBe` 180
    it "maximum" $ do
      maximum (toList (testLists !! 0)) `shouldBe` 4
      maximum (toList (testLists !! 1)) `shouldBe` 5
      maximum (toList (testLists !! 2)) `shouldBe` 553
      maximum (toList (testLists !! 3)) `shouldBe` 0
      maximum (treeTestList !! 5) `shouldBe` 6
    it "minimum" $ do
      minimum (toList (testLists !! 0)) `shouldBe` 4
      minimum (toList (testLists !! 1)) `shouldBe` 1
      minimum (toList (testLists !! 2)) `shouldBe` 2
      minimum (toList (testLists !! 3)) `shouldBe` 0
      minimum (treeTestList !! 5) `shouldBe` 5
