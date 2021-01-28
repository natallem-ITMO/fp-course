module Block1.Task3
  ( 
    NonEmpty (..)
  ) where

import Control.Applicative (liftA2)

-- | Representation of non empty list (always has at least
--   one argument)
data NonEmpty a = a :| [a]
  deriving (Show, Eq)

instance Functor NonEmpty where
  fmap f (a :| b) = f a :| fmap f b

instance Applicative NonEmpty where
  pure x = x :| []
  (f :| fs) <*> (x :| xs) = f x :| (map f xs ++ map ($x) fs ++ (fs <*> xs))

instance Foldable NonEmpty where
  foldr f acc (x :| xs) = foldr f acc (x : xs)

instance Traversable NonEmpty where
  traverse f (a :| b) = liftA2 (:|) (f a) $ traverse f b
