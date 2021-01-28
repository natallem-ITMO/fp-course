{-# LANGUAGE InstanceSigs #-}
module Comonads.Comonads where
import Control.Comonad

data ListZipper a = LZ [a] a [a]
    deriving Show

listLeft, listRight ::  ListZipper a -> ListZipper a
listLeft (LZ (x : xs) e (y : ys)) = LZ (e : x : xs) y ys 
listLeft _ = error "Most left"

listRight (LZ (x : xs) e (y : ys)) = LZ xs e (e : y : ys)
listRight _ = error "Most right"

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ xs _ ys) = LZ xs  x ys

instance Functor ListZipper where
    fmap :: (a -> b)  -> ListZipper a -> ListZipper b
    fmap f (LZ xs e ys) = LZ (map f xs) (f e) (map f ys)

instance Comonad ListZipper where
    extract :: ListZipper a -> a
    extract (LZ _ y _) = y

    duplicate :: ListZipper a -> ListZipper (ListZipper a)
    duplicate cur = LZ  (iterateTail listLeft cur) cur  (iterateTail listRight cur)

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

example :: ListZipper Int 
example = LZ [1,2,3,4] 5 [6,7,8]






    



 
