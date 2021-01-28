{-# LANGUAGE InstanceSigs #-}
module Block1.Task3
  (
   Tree (..)
  , deleteElementTree
  , insertTree
  , isEmptyTree
  , findTree
  , fromList
  , sizeTree
  ) where
  
import Data.List.NonEmpty (NonEmpty (..), head, tail, toList)
import Prelude hiding (head, tail)
import qualified Prelude as P (head, tail)

-- | Representation of Tree struct.
-- Each node in Tree is List(empty)
-- or Node with not empty list of
-- same values and two children nodes.
-- Invariant: for each node except List
-- all values in all nodes in left child are
--  less then current value in parent node,
--  and all values in all nodes in right
--  child are greater then current node value
data Tree a = List | Root (NonEmpty a) (Tree a) (Tree a)
  deriving (Show)

-- | Tree == Tree if they have same
--  structure and values in every node
instance Eq a => Eq (Tree a) where
  (==) List List = True
  (==) (Root a b c) (Root x y z) = (a == x) && (b == y) && (c == z)
  (==) _ _ = False

-- | Insert element in Tree of elements,
-- that can be ordered. Return new Tree with
-- inserted element
insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree a List = Root (a :| []) List List
insertTree a (Root list left right)
  | a == head list =
    Root (a :| [head list] ++ tail list) left right
  | a < head list = Root list (insertTree a left) right
  | otherwise = Root list left (insertTree a right)

-- | Return number of element in list of every node
sizeTree :: Tree a -> Int
sizeTree List = 0
sizeTree (Root list left right) =
  length list + sizeTree left + sizeTree right

-- | Return True if Tree is empty (consists
-- of List only), False otherwise
isEmptyTree :: Tree a -> Bool
isEmptyTree List = True
isEmptyTree Root {} = False

-- | Return True if in given Tree exists
-- node with list, containing given value,
-- otherwise return False. Effectively
-- uses binary search tree invariant for.
findTree :: (Ord a) => a -> Tree a -> Bool
findTree _ List = False
findTree a (Root list left right)
  | a == head list = True
  | a < head list = findTree a left
  | otherwise = findTree a right

-- | Return new Tree with deleted given element
-- if such element is represented in given Tree.
-- Delete only one element, remaining Node
-- (if deleted element not one). If no such
-- element in Tree, return given Tree
deleteElementTree :: (Ord a) => a -> Tree a -> Tree a
deleteElementTree _ List = List
deleteElementTree a (Root list left right)
  | a == head list =
    case (length list) of
      1 -> case left of
        List -> List
        _    -> Root (findMostLeft left) (deleteMostLeft left) right
      _ -> Root (P.head (tail list) :| P.tail (tail list)) left right
  | a < head list = Root list (deleteElementTree a left) right
  | otherwise = Root list left (deleteElementTree a right)


-- | Return the NonEmpty list of the biggest
--  element of all elements that less then
--  or equals current Node element (aka
-- most left element starts from current Node).
-- Call only from deleteElementTree function,
-- thus cannot be called on List(due to inv.)
findMostLeft :: (Eq a) => Tree a -> NonEmpty a
findMostLeft (Root list _ right)
  | right == List = list
  | otherwise = findMostLeft right
findMostLeft List = undefined

-- | Return Tree without
-- most left element starts from current Node.
-- Call only from deleteElementTree function,
-- thus cannot be called on List(due to inv.)
deleteMostLeft :: (Eq a) => Tree a -> Tree a
deleteMostLeft (Root list left right)
  | right == List = left
  | otherwise = Root list left (deleteMostLeft right)
deleteMostLeft List = undefined

-- | Return Tree, containing all elements
--  of given list (and only them)
fromList :: (Ord a) => [a] -> Tree a
fromList = foldl (flip insertTree) List


instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z List = z
  foldr func acc (Root list left right) = foldLeftSubTree (foldNode foldRightSubTree)
    where
      foldRightSubTree = foldr func acc right
      foldNode resRight = foldr func resRight (toList list)
      foldLeftSubTree resOther = foldr func resOther left

  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ List = mempty
  foldMap f (Root list left right) =
    foldMap f left
      `mappend` mconcat (map f (toList list))
      `mappend` foldMap f right
