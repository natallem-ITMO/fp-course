{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module DataKind.TypeLits where

import GHC.TypeLits 

data Vec :: * -> Nat -> * where
    Nil :: Vec a 0
    (:>) :: a -> Vec a n -> Vec a (n+1) --(+) :: Nat -> Nat -> Nat

-- type family 

newtype Foo bar = MkFoo{unFoo :: bar}
--  берет тип bar и возвращает новый тип 
-- :k Foo = * -> *

-- data Foo2 a where
    -- Foo2 :: Double -> Foo Double 
    -- Bar :: Char -> Foo String 

type family Foo1 bar :: *  where
    Foo1 Char = Double 
    Foo1 b = b


type family Foo2 bar :: *
type instance Foo2 Char = Double 
type instance Foo2 Int = Int  -- type family - функции на типах 



    

    






