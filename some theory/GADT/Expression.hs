{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
module GADT.Expression where

import           Data.Typeable


data ArithmExpr a where
    AENum :: Int -> ArithmExpr Int
    AEPlus :: ArithmExpr Int -> ArithmExpr Int -> ArithmExpr Int
    AEAnd :: ArithmExpr Bool -> ArithmExpr Bool -> ArithmExpr Bool
    AEGt ::  ArithmExpr Int -> ArithmExpr Int -> ArithmExpr Bool


interpret :: ArithmExpr a -> a
interpret (AENum a)    = a
interpret (AEPlus a b) = interpret a +  interpret b
interpret (AEAnd a b)  = interpret a && interpret b
interpret (AEGt a b)   = interpret a  > interpret b

instance Show (ArithmExpr a) where
    show (AENum a)    = show a
    show (AEPlus a b)=show a ++ "+" ++ show b
    show (AEAnd a b)= show a ++ "&&" ++ show b
    show (AEGt a b)=  show a ++ ">" ++ show b


data TT where
     TT :: Show a => ArithmExpr a -> TT

data TT1 =
   forall a.(Show a) => TT1 (ArithmExpr a)  -- forall is equvivalent as above

instance Show (TT1) where
    show (TT1 a) = show a


data SomeAE where
    SomeAE :: (Show a, Typeable a) => ArithmExpr a -> SomeAE


-- data a :~: b where
    -- Reft :: a :~: a

-- class Typeable (a :: k) where

-- eqT :: forall a b. (Typeable  a, Typeable b)=> Maybe (a :~: b)

parse :: String -> Maybe SomeAE
parse "1+2" = Just (SomeAE $ AEPlus (AENum 1) (AENum 2))
parse "1"   = Just (SomeAE $ AENum 1)
parse _     = Nothing

parseInt :: String -> Maybe ( ArithmExpr Int)
parseInt str = parse str >>= \(SomeAE (expr :: ArithmExpr t)) ->
         do
            Refl <- eqT @t @Int
            pure expr


class AExpression expr where
    aenum :: Int -> expr Int
    aeplus :: expr Int -> expr Int -> expr Int
    aeand :: expr Bool -> expr Bool -> expr Bool
    aegt :: expr Int -> expr Int -> expr Bool

myEprEx :: AExpression expr => expr Int
myEprEx = aenum 3

newtype ToS a = ToS{toString :: String}
    deriving(Show, Semigroup )


castTS :: ToS a -> ToS b
castTS (ToS s) = ToS s

instance AExpression ToS where
    aenum i = ToS $ show i
    aeplus a b = a <> ToS " + " <>b
    aeand a b = a <> ToS " && " <>b
    aegt a b = castTS a <> ToS " && " <> castTS b

exAExpression ::( AExpression e) => e Int
exAExpression = aenum 3 `aeplus` aenum 3

exAExpression2 ::( AExpression e) => e Bool
exAExpression2 = aenum 3 `aeplus` aenum 3 `aegt` aenum 1
hahah :: String
hahah = toString (aenum 3 `aeplus` aenum 3)

newtype ToV a = ToV{toValue :: a}

instance AExpression ToV where
    aenum = ToV
    aeplus a b = ToV $( toValue a) + toValue b
    aeand a b = ToV $( toValue a) && toValue b
    aegt a b = ToV $( toValue a) > toValue b



