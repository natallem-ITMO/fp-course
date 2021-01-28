{-# LANGUAGE InstanceSigs #-}

module Learnyouahaskell.WriterMonad where
import Control.Monad.Writer


{-newtype Writer w a = Writer { runWriter :: (a, w) }
instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')-}

--runWriter (return 3 :: Writer (Sum Int) Int)
--(3,Sum {getSum = 0})

--logNumber :: Int -> Writer [String] Int
--logNumber x = Writer(x, ["Got number: " ++ show x])

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a*b)

--multWithLog2 :: Writer [String] Int
--multWithLog2 =  logNumber 3 >>= \a -> 
--                logNumber 5 >>= \b ->
--                tell ["Gonna mul"] >>= t ->
--                return (a*b)
multWithLog2 = logNumber 3 >>= \a ->
                logNumber 5 >>= \b ->
                tell ["gn"] >>
                return (a*b)

multWithLog3 = do  
    a <- logNumber 3  
    b <- logNumber 5  
    _ <- tell ["Gonna multiply these two"]  
    return (a*b) 
    
{- logs stuff in reverse. First it produces the log for the rest of the procedure and then adds the current step to the 
end of the log. It does the recursion first, and binds its result value to result. Then it adds the current step to the 
log, but the current step goes at the end of the log that was produced by the recursion.-} 
gcdReverse :: Int -> Int -> Writer [String] Int  
gcdReverse a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        result <- gcdReverse b (a `mod` b)  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        return result  


