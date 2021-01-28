{-# LANGUAGE InstanceSigs #-}

module Learnyouahaskell.ReaderMonad where

import Control.Monad.Instances
import Control.Monad.Reader

{-Offtop-}
{-
(->) r is an instance of Functor. Mapping a function f over a function g will make a function that takes the same thing
as g, applies g to it and then applies f to that result. So basically, we're making a new function that's like g, only
before returning its result, f gets applied to that result as well.


We've also seen that functions are applicative functors. They allow us to operate on the eventual results of
functions as if we already had their results.
-}
f = (+) <$> (*2) <*> (+10)
{-
ghci> f 3
19
-}

{- a function can also be considered a value with a context. The context for functions is that that value is not
present yet and that we have to apply that function to something in order to get its result value.-}

{-
instance Monad ((->) r) where
  return x = \_ -> x
  h >>= f = \w -> f (h w) w
  -}


addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a+b)

{-По сути мы к каждому значению применяем нашу новую монадофункцию(частичное применение), т е сохраняем исходное значение
и что то с ним постоянно делаем. сначала домножаем на 2. потом прибавляем 10, что то типа композиции
-}
addStuff2 :: Int -> Int
addStuff2 = (*2) >>= \a -> (+10) >>= \b -> return (a+b)

{-
 function monad is also called the reader monad. All the functions read from a common source. To illustrate this even better, we can rewrite addStuff like so:
-}

addStuff3 :: Int -> Int
addStuff3 x = let
    a = (*2) x
    b = (+10) x
    in a+b

{-
We can act as if we already know what the functions will return. It does this by gluing functions together into one
 function and then giving that function's parameter to all of the functions that it was glued from.
-}

{-
Conception of Reader Monad - thing that permits to pass arguments through all call stack without implicitly initialization
this argument

newtype Reader e a = Reader { runReader :: e -> a }

instance Monad (Reader e) where
    return :: a -> Reader e a
    return a = Reader $ \_ -> a

    (>>=) :: Reader e a -> (a -> Reader e b) -> Reader e b
    m >>= f = Reader $ \r -> runReader (f $ runReader m r) r

Useful functions-}

{-
ask:: Reader e e
asks :: (e -> a) -> Reader e a -- pass function that get value from context and return this value a in monad
local :: (e -> b) -> Reader b a  -> Reader e a -- change context (don't know how it works, not explained)
-}

data Environment = Environment { ids  :: [Int]
                               , name :: Int -> String
                               , near :: Int -> (Int, Int) }
inEnv :: Int -> Reader Environment Bool
inEnv i = asks (elem i . ids)

