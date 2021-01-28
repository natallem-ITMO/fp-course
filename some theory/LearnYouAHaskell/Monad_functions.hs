{-# LANGUAGE InstanceSigs #-}

module Learnyouahaskell.Monad_functions where

import Control.Monad.Error
import Control.Monad.Writer
import Control.Monad
{-
1)
liftM == fmap
historically Monad wasn't necessary Functor. But now Functor -> Applicative -> Monad.
Do not use liftM (fmap preferable)

liftM :: (Monad m) => (a -> b) -> m a -> m b
fmap :: (Functor f) => (a -> b) -> f a -> f b
-}
{-
ghci> liftM (*3) (Just 8)
Just 24
ghci> fmap (*3) (Just 8)
Just 24
ghci> runWriter $ liftM not $ Writer (True, "chickpeas")
(False,"chickpeas")
ghci> runWriter $ fmap not $ Writer (True, "chickpeas")
(False,"chickpeas")
ghci> runState (liftM (+100) pop) [1,2,3,4]  -- see StateMonad
(101,[2,3,4])
ghci> runState (fmap (+100) pop) [1,2,3,4]
(101,[2,3,4])
-}

{-
how liftM implemented
-}
{-
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= (\x -> return (f x))

liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = do
      x <- m
      return (f x)
-}

{-
2)
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = do
    f <- mf
    x <- m
    return (f x)

ghci> Just (+3) <*> Just 4
Just 7
ghci> Just (+3) `ap` Just 4
Just 7
ghci> [(+1),(+2),(+3)] <*> [10,11]
[11,12,12,13,13,14]
ghci> [(+1),(+2),(+3)] `ap` [10,11]
[11,12,12,13,13,14]
-}

{-
3)liftM2 / liftM3 / liftM4 / liftM5
same as:ж
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y
-}

{-
4) join
join :: (Monad m) => m (m a) -> m a
join mm = do
    m <- mm
    m

Examples:

ghci> join (Just (Just 9))
Just 9
ghci> join (Just Nothing)
Nothing

ghci> join [[1,2,3],[4,5,6]]
[1,2,3,4,5,6]

ghci> runWriter $ join (Writer (Writer (1,"aaa"),"bbb"))
(1,"bbbaaa")

ghci> join (Right (Right 9)) :: Either String Int
Right 9
ghci> join (Right (Left "error")) :: Either String Int
Left "error"

ghci> runState (join (state $ \s -> (push 10,1:2:s))) [0,0,0]
((),[10,1,2,0,0,0])
--state in state because state $ \s -> state

NB : m >>= f always equals join (fmap f m)
-}

{-
5) filterM
filter :: (a -> Bool) -> [a] -> [a]
ghci> filter (\x -> x < 4) [9,1,5,2,10,3]
[1,2,3]

filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
-}
keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Keeping " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " is too large, throwing it away"]
        return False
{-
ghci> fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3]
[1,2,3]

ghci> mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]
9 is too large, throwing it away
Keeping 1
5 is too large, throwing it away
Keeping 2
10 is too large, throwing it away
Keeping 3

powerset :: [a] -> [[a]]  -- sets of all subsets
powerset xs = filterM (\x -> [True, False]) xs

ghci> powerset [1,2,3]
[[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
-}

{-
6)foldM
foldl :: (a -> b -> a) -> a -> [b] -> a
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
    | x > 9     = Nothing
    | otherwise = Just (acc + x)

ghci> foldM binSmalls 0 [2,8,3,1]
Just 14
ghci> foldM binSmalls 0 [2,11,3,1]
Nothing

-}















