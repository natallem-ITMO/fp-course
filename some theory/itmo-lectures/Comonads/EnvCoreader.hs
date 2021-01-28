{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
module Comonads.EnvCoreader where
import Control.Comonad
type Pos1D = Env Int Int 
-- extract :: Pos1D -> Int 
-- extract  = snd 

-- extend :: ((e,a) -> b) -> (e, a) -> (e, b) 
-- extend f env = (fst env, f env)

start :: Int -> Pos1D
start a = Env  a a

left, right :: Int -> Pos1D -> Pos1D
left i (Env x n ) = Env x (n-i)
right i (Env x n ) = Env x (n + i)

data Env e a = Env e a 
    deriving Functor

instance Comonad (Env e) where
    extract :: Env e a -> a
    extract (Env _ a) = a

    extend :: (Env e a -> b) -> Env e a -> Env e b 
    extend f env@(Env e a) = Env e (f env)

-- toStart :: Pos1D -> Pos1D 
-- toStart (z,x) = let d =  if abs(z-x) >= 10 then z else x
--     in (d, x) 

-- safeRight :: Int -> Pos1D -> Int 
-- safeRight n p = extract $ p =>> right n

toStart :: Pos1D -> Int
toStart (Env z x) = if abs (z - x) >= 10 then z else x

rightEnv :: Int -> Pos1D -> Int 
rightEnv n (Env x y) = y + n

safeRight :: Int -> Pos1D -> Int
safeRight n p = extract $ p =>> rightEnv n 




