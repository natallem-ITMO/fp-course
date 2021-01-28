{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module DataKind.FreeMonad where

data Free f a = Pure a | Free (f (Free f a))

data Action a = 
    PrintLn String a | ReadInt (Int -> a) -- т е PrintLn String a            ReadInt (no arg) (Int as result -> a)
    deriving (Functor)                    --             Arg1 (no res -> a)

justPrint :: Free Action Int
justPrint = Free $ PrintLn "hello" (Pure 5)

readIncPrint :: Free Action ()
readIncPrint = 
    Free $ ReadInt $ \i -> Free $ PrintLn (show $ i + 1) (Pure ())

printLn :: String -> Free Action ()
printLn s = Free $ PrintLn s (Pure ())

readInt :: Free Action Int 
readInt = Free $ ReadInt Pure

-- readIncPrintMonad :: Free Action ()
-- readIncPrintMonad = do
    -- i <- readInt 
    -- printLn $ show (i+1)

runAction :: Free Action a -> IO a
runAction (Pure a ) = pure a 
runAction (Free (PrintLn str cont)) =  do 
    putStrLn str 
    runAction cont 
runAction (Free (ReadInt f)) = do
    i <- fmap read getLine 
    runAction (f i)

-- instance Functor f  => Monad (Free f) where
    -- return = pure 
    -- Pure a >>= f = f a
    -- (Free m) >>= f = Free $ (>>= f) <$> m
