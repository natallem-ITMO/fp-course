{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module DataKind.DataKinds  where


data Nat = Z | S Nat

data Vec :: * -> Nat -> * where
    Nil :: Vec a 'Z
    (:>) :: a -> Vec a n -> Vec a (S n)

zipV :: Vec a n -> Vec b n -> Vec (a,b) n
zipV Nil Nil = Nil
zipV (x :> xs) (y :> ys) = (x,y) :> zipV xs ys

xVec :: Vec Int (S Z)
xVec = 3 :> Nil

yVec :: Vec Bool (S Z)
yVec = True :> Nil

xyVec :: Vec (Int, Bool) (S Z)
xyVec = zipV xVec yVec

data HList :: [*] -> * where
    HNil :: HList '[]
    (:^) :: a -> HList t -> HList (a ': t) 

infixr 2 :^

foo :: HList '[]
foo = HNil

foo1 :: HList '[Int]
foo1 = 4 :^ HNil

foo2 :: HList '[Int, Bool]
foo2 = 4 :^ True :^ HNil

instance Show (HList '[]) where
    show _ = "H[]"

instance (Show e, Show (HList l)) => Show (HList (e ': l)) where
    show (x :^ l ) = let 'H':'[' : s = show l
                    in "H[" ++ show x ++ (if s == "]" then s else ", " ++ s)


type X = 2 




