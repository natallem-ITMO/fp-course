module Block1.Task2
  (
    Tree (..)
  ) where

import Control.Applicative (liftA2)

-- | Representation of a binary tree structure
data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a
  deriving (Show, Eq)

instance Functor Tree where
  fmap f (Branch a b) = Branch (fmap f a) (fmap f b)
  fmap f (Leaf a) = Leaf (f a)

instance Applicative          Tree where
  pure = Leaf
  (Leaf f) <*> (Leaf a) = Leaf (f a)
  (Leaf f) <*> (Branch a b) = Branch (fmap f a) (fmap f b)
  (Branch f1 f2) <*> (Branch f1' f2') = Branch (f1 <*> f1') (f2 <*> f2')
  (Branch f1 f2) <*> leaf = Branch (f1 <*> leaf) (f2 <*> leaf)

-- | instance of Foldable for Tree data type
-- folds tree from one side to another recursively
instance Foldable Tree where
  foldr f acc (Leaf a) = f a acc
  foldr f acc (Branch a b) = foldr f foldRight a
    where
      foldRight = foldr f acc b

instance Traversable Tree where
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Branch a b) = liftA2 Branch (traverse f a) (traverse f b)
