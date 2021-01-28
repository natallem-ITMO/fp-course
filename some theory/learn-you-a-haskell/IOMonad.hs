{-# LANGUAGE InstanceSigs #-}

module Learnyouahaskell.IOMonad where
import Data.Char
import Control.Monad

main2 :: IO ()
main2 = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName -- use let in expression without in to bind pure calculation to name
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

main3 :: IO ()
main3 = do
    line <- getLine
    if null line
        then return ()  -- use to return from recursion
        else do
            putStrLn $ reverseWords line
            main3


main3_1 = do
    c <- getChar
    when (c /= ' ') $ do  -- import Control.Monad,
                          -- . It takes a boolean value and an I/O action if that boolean value is True, it returns the
                          -- same I/O action that we supplied to it. However, if it's False, it returns the return (),
                          -- action, so an I/O action that doesn't do anything.
        putChar c
        main3_1

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

{-
putStrLn - print passed string (side IO effect), result of this actions is IO () (nothing)
print = putStrLn . show
show = represent object as string

ghci > print "hello"
"hello"

ghci > putStrLn "hello"
hello


ghci > show "hello"
"\"hello\""  -- as show hello is "\"hello\"" and we print it

-}

{-
sequence takes a list of I/O actions and returns an I/O actions that will perform those actions one after the other.
The result contained in that I/O action will be a list of the results of all the I/O actions that were performed.
Its type signature is sequence :: [IO a] -> IO [a]. Doing this:
-}

main4 :: IO ()
main4 = do
  rs <- sequence[getLine, getLine]
  print rs