{-# LANGUAGE InstanceSigs #-}

module Learnyouahaskell.ListMonad where

import Control.Monad
{-instance Monad [] where
      return x = [x]
      xs >>= f = concat (map f xs)
      fail _ = [] -}
arr::[Int]
arr = [3,4,5] >>= \x -> [x,-x]

arr1::[[Char]]
arr1 = [] >>= \x -> ["bad","mad","rad"]
--       []

arr2::[(Int, Char)]
arr2= [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)
--[(1,'a'),(1,'b'),(2,'a'),(2,'b')]

listOfTuples :: [(Int,Char)]
listOfTuples = do -- syntactic sugar is list comprehension [ (n,ch) | n <- [1,2], ch <- ['a','b'] ]
  x <- [1,2]
  y <- ['a','b']
  return (x,y)

-- filtering?

{-class Monad m => MonadPlus m where --Control.Monad
    mzero :: m a
    mplus :: m a -> m a -> m a  -}

{-instance MonadPlus [] where
      mzero = []
      mplus = (++) -}

{-guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero  -}

--guard (5 > 2) :: Maybe ()
--Just ()
--ghci> guard (1 > 2) :: Maybe ()
--Nothing
--ghci> guard (5 > 2) :: [()]
--[()]
--ghci> guard (1 > 2) :: [()]

ex :: [Int]
ex = [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
ex1 :: [Int]
ex1 = do
  x <- [1..50]
  guard ('7' `elem` show x) --  _ <- guard ('7' `elem` show x)
  return x

--ghci> guard (5 > 2) >> return "cool" :: [String]
--["cool"]
--ghci> guard (1 > 2) >> return "cool" :: [String]
--[]