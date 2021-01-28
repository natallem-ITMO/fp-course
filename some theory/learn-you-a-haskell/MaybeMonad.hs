{-# LANGUAGE InstanceSigs #-}

module Learnyouahaskell.MaybeMonad where

{-instance Monad Maybe where  
      return x = Just x  
      Nothing >>= f = Nothing  
      Just x >>= f  = f x  
      fail _ = Nothing  -}
      
type Birds = Int  
type Pole = (Birds,Birds)  

landLeft :: Birds -> Pole -> Maybe Pole  
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing  
  
landRight :: Birds -> Pole -> Maybe Pole  
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing  

banana :: Pole -> Maybe Pole  
banana _ = Nothing  

bindOperatorEx :: Maybe String
bindOperatorEx = Just (3::Int) >>= (\x 
                  -> Just "!" >>= (\y 
                  -> Just (show x ++ y))) 
{- \x ->
        <<x == Int>>
        Just "!" >>= (
                  \y 
                  <<y == String>> 
                      -> Just (show x ++ y))

simular to 
              let x = 3; y = "!" in show x ++ y  
             "3!"  -}
             
bindOperatorEx2 :: Maybe String
bindOperatorEx2 = do
  x <- Just (3::Int)
  y <- Just "!"
  Just (show x ++ y)

routine :: Maybe Pole  
routine = do  
    start <- return (0,0)  
    first <- landLeft 2 start  
    second <- landRight 2 first  
    landLeft 1 second  
    
routine1 = do  
    start <- return (0,0)  
    first <- landLeft 2 start  
    Nothing  
    second <- landRight 2 first  
    landLeft 1 second 
    
    
