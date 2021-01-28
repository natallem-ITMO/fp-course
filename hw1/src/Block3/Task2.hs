module Block3.Task2
  (
    NonEmpty(..)
  , ThisOrThat(..)
  ) where
  
import Prelude hiding (head, tail)
-- | Realization for NonEmpty to
--  implement instance Semigroup
data NonEmpty a = a :| [a]
  deriving (Show)
  
-- | Eq instance using in tests
instance Eq a => Eq (NonEmpty a) where
  (==) (a :| b) (c :| d) = (a == c) && (b == d)

-- | Return first element of NonEmpty
head :: NonEmpty a -> a
head (a :| _) = a

-- | Return list of tail elements of
--  NonEmpty(might be empty)
tail :: NonEmpty a -> [a]
tail (_ :| t) = t

-- | <> - basically concat two NonEmpty lists
instance Semigroup (NonEmpty a) where
  (<>) x y = head x :| (tail x ++ [head y] ++ tail y)
    
-- | Data for 
data ThisOrThat a b = This a | That b | Both a b
  deriving (Show)

-- | Using in tests
instance (Eq a, Eq b) => Eq (ThisOrThat a b) where
  (==) (This a) (This b) = a == b
  (==) (This _) _ = False
  (==) (That a) (That b) = a == b
  (==) (That _) _ = False
  (==) (Both a b) (Both c d) = (a == c) && (b == d)
  (==) (Both _ _) _ = False

-- | <> - basically return Both if have 
-- This and That. Priority to the first 
-- This That element
instance Semigroup (ThisOrThat a b) where
  (<>) (This a) (This _) = This a
  (<>) (This a) (That b) = Both a b
  (<>) (This a) (Both _ c) = Both a c
  (<>) (That a) (This b) = Both b a
  (<>) (That a) (That _) = That a
  (<>) (That a) (Both b _) = Both b a
  (<>) thisOrThat@(Both _ _) _ = thisOrThat
