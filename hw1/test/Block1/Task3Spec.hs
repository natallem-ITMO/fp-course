module Block1.Task3Spec
  ( 
     spec
  ,  treeTestList
  ,  testLists
  )  where

import Data.Foldable as Fold (toList)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..), head, nub)
import Prelude hiding (head, tail)

import Block1.Task3 (Tree (..), deleteElementTree, findTree, fromList, insertTree, isEmptyTree, sizeTree)
  
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
  describe "insertTree" $ do
    it "insertCheckTreeStructure" $ do
      insertTree 5 (treeTestList !! 0) `shouldBe` treeTestList !! 1
      insertTree 5 (treeTestList !! 1) `shouldBe` treeTestList !! 2
      insertTree 6 (treeTestList !! 1) `shouldBe` treeTestList !! 3
      insertTree 6 (treeTestList !! 3) `shouldBe` treeTestList !! 4
      insertTree 6 (treeTestList !! 4) `shouldBe` treeTestList !! 5
      insertTree 3 (treeTestList !! 5) `shouldBe` treeTestList !! 6
      insertTree 3 (treeTestList !! 6) `shouldBe` treeTestList !! 7
      insertTree 3 (treeTestList !! 7) `shouldBe` treeTestList !! 8
      insertTree 5 (treeTestList !! 8) `shouldBe` treeTestList !! 9
      insertTree 2 (treeTestList !! 9) `shouldBe` treeTestList !! 10
      insertTree 2 (treeTestList !! 10) `shouldBe` treeTestList !! 11
      insertTree 8 (treeTestList !! 12) `shouldBe` treeTestList !! 13
      insertTree 10 (treeTestList !! 11) `shouldBe` treeTestList !! 12
    it "insertNotCheckStructure" $ do
      foldTree (insertTree (-4) (List :: Tree Integer))
        `shouldBe` [-4]
      insertTree (-4) (List :: Tree Integer)
        `shouldSatisfy` correctTree
      foldTree (insertTree 11 (treeTestList !! 12))
        `shouldBe` [2, 2, 3, 3, 3, 5, 5, 6, 6, 6, 10, 11]
      insertTree 11 (treeTestList !! 12)
        `shouldSatisfy` correctTree
      foldTree (insertTree 1 (treeTestList !! 13))
        `shouldBe` [1, 2, 2, 3, 3, 3, 5, 5, 6, 6, 6, 8, 10]
      insertTree 1 (treeTestList !! 13)
        `shouldSatisfy` correctTree
      foldTree (insertTree 4 (treeTestList !! 13))
        `shouldBe` [2, 2, 3, 3, 3, 4, 5, 5, 6, 6, 6, 8, 10]
      insertTree 1 (treeTestList !! 13)
        `shouldSatisfy` correctTree
      foldTree (insertTree 7 (treeTestList !! 13))
        `shouldBe` [2, 2, 3, 3, 3, 5, 5, 6, 6, 6, 7, 8, 10]
      insertTree 1 (treeTestList !! 13)
        `shouldSatisfy` correctTree
      foldTree (insertTree (-5) (treeTestList !! 13))
        `shouldBe` [-5, 2, 2, 3, 3, 3, 5, 5, 6, 6, 6, 8, 10]
      insertTree 1 (treeTestList !! 13)
        `shouldSatisfy` correctTree
      foldTree (insertTree 3 (treeTestList !! 13))
        `shouldBe` [2, 2, 3, 3, 3, 3, 5, 5, 6, 6, 6, 8, 10]
      insertTree 1 (treeTestList !! 13)
        `shouldSatisfy` correctTree
      foldTree (insertTree 9 (treeTestList !! 13))
        `shouldBe` [2, 2, 3, 3, 3, 5, 5, 6, 6, 6, 8, 9, 10]
      insertTree 1 (treeTestList !! 13)
        `shouldSatisfy` correctTree
  describe "deleteElement" $ do
    it "deleteCheckStructure" $ do
      deleteElementTree 5 (treeTestList !! 0)
        `shouldBe` (treeTestList !! 0)
      deleteElementTree 5 (treeTestList !! 1)
        `shouldBe` (treeTestList !! 0)
      deleteElementTree 5 (treeTestList !! 2)
        `shouldBe` (treeTestList !! 1)
      deleteElementTree 3 (treeTestList !! 7)
        `shouldBe` (treeTestList !! 6)
      deleteElementTree 2 (treeTestList !! 11)
        `shouldBe` (treeTestList !! 10)
    it "deleteNotCheckStructure" $ do
      foldTree (deleteElementTree 3 (treeTestList !! 12))
        `shouldBe` [2, 2, 3, 3, 5, 5, 6, 6, 6, 10]
      deleteElementTree 3 (treeTestList !! 12)
        `shouldSatisfy` correctTree
      foldTree (deleteElementTree 1 (treeTestList !! 12))
        `shouldBe` [2, 2, 3, 3, 3, 5, 5, 6, 6, 6, 10]
      deleteElementTree 1 (treeTestList !! 12) `shouldSatisfy` correctTree
      foldTree (deleteElementTree 10 (treeTestList !! 12))
        `shouldBe` [2, 2, 3, 3, 3, 5, 5, 6, 6, 6]
      deleteElementTree 10 (treeTestList !! 12)
        `shouldSatisfy` correctTree
  describe "others operations" $ do
    it "size" $ do
      sizeTree (treeTestList !! 0) `shouldBe` 0
      sizeTree (treeTestList !! 1) `shouldBe` 1
      sizeTree (treeTestList !! 2) `shouldBe` 2
      sizeTree (treeTestList !! 10) `shouldBe` 9
      sizeTree (treeTestList !! 12) `shouldBe` 11
    it "isEmpty" $ do
      isEmptyTree (treeTestList !! 0) `shouldBe` True
      isEmptyTree (treeTestList !! 2) `shouldBe` False
      isEmptyTree (treeTestList !! 4) `shouldBe` False
    it "findInTree" $ do
      findTree 5 (treeTestList !! 0) `shouldBe` False
      findTree 5 (treeTestList !! 1) `shouldBe` True
      findTree 6 (treeTestList !! 1) `shouldBe` False
      findTree 13 (treeTestList !! 13) `shouldBe` False
      findTree 6 (treeTestList !! 13) `shouldBe` True
      findTree 8 (treeTestList !! 13) `shouldBe` True
      findTree (-1) (treeTestList !! 13) `shouldBe` False
    it "fromList" $ do
      fromList [] `shouldBe` treeTestList !! 0
      fromList [5] `shouldBe` treeTestList !! 1
      fromList [5, 5] `shouldBe` treeTestList !! 2
      fromList (testLists !! 0) `shouldSatisfy` correctTree
      fromList (testLists !! 1) `shouldSatisfy` correctTree
      fromList (testLists !! 2) `shouldSatisfy` correctTree
      foldTree (fromList (testLists !! 0)) `shouldBe` sort (testLists !! 0)
      foldTree (fromList (testLists !! 1)) `shouldBe` sort (testLists !! 1)
      foldTree (fromList (testLists !! 2)) `shouldBe` sort (testLists !! 2)
      foldTree (fromList (testLists !! 3)) `shouldBe` sort (testLists !! 3)

treeTestList :: [Tree Int]
treeTestList =
  [ List, -- 0
    Root (5 :| []) List List, -- 1
    Root (5 :| [5]) List List, -- 2
    Root (5 :| []) List (Root (6 :| []) List List), -- 3
    Root (5 :| []) List (Root (6 :| [6]) List List), -- 4
    Root (5 :| []) List (Root (6 :| [6, 6]) List List), -- 5
    Root
      (5 :| [])
      (Root (3 :| []) List List)
      (Root (6 :| [6, 6]) List List), -- 6
    Root
      (5 :| [])
      (Root (3 :| [3]) List List)
      (Root (6 :| [6, 6]) List List), -- 7
    Root
      (5 :| [])
      (Root (3 :| [3, 3]) List List)
      (Root (6 :| [6, 6]) List List), -- 8
    Root
      (5 :| [5])
      (Root (3 :| [3, 3]) List List)
      (Root (6 :| [6, 6]) List List), -- 9
    Root
      (5 :| [5])
      (Root (3 :| [3, 3]) (Root (2 :| []) List List) List)
      (Root (6 :| [6, 6]) List List), -- 10
    Root
      (5 :| [5])
      (Root (3 :| [3, 3]) (Root (2 :| [2]) List List) List)
      (Root (6 :| [6, 6]) List List), -- 11
    Root
      (5 :| [5])
      (Root (3 :| [3, 3]) (Root (2 :| [2]) List List) List)
      (Root (6 :| [6, 6]) List (Root (10 :| []) List List)), -- 12
    Root
      (5 :| [5])
      (Root (3 :| [3, 3]) (Root (2 :| [2]) List List) List)
      (Root (6 :| [6, 6]) List (Root (10 :| []) (Root (8 :| []) List List) List)) -- 13
  ]
  
-- | Test list of lists to construct Tree
testLists :: [[Double]]
testLists =
  [ [4],
    [1, 4, 5, 3, 2],
    [2, 3, 43, 2, 53, 553, 5, 5, 5, 5, 3, 2, 3],
    [0, 0, 0, 0, 0, 0, 0, 0]
  ]
  
-- | Checks if given tree correspond to inv. 
-- (described  in data Tree comments)
correctTree :: (Ord a) => Tree a -> Bool
correctTree List = True
correctTree (Root list left right)
  | length unique == 1
      && maybe True (curValue >) (minOrMaxInTree max left)
      && maybe True (curValue <) (minOrMaxInTree min right) =
    correctTree left && correctTree right
  | otherwise = False
  where
    unique = nub list
    curValue = head list
    
-- | Find some most (min/max) element in Tree 
-- (if Tree has at list one node, otherwise 
-- return Nothing)
minOrMaxInTree :: (a -> a -> a) -> Tree a -> Maybe a
minOrMaxInTree _ List = Nothing
minOrMaxInTree func (Root list left right) =
  Just $ func (thisOrOtherMax leftMax) (thisOrOtherMax rightMax)
  where
    thisMax = head list
    leftMax = minOrMaxInTree func left
    rightMax = minOrMaxInTree func right
    thisOrOtherMax other = maybe thisMax (func thisMax) other

-- | Fold all value in Tree
--  (sorted order) to check
foldTree :: Tree a -> [a]
foldTree = Fold.toList
