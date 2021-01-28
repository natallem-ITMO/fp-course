{-# LANGUAGE InstanceSigs #-}

module Learnyouahaskell.StateMonad where

import Control.Monad.State.Lazy


{-Offtop-}
{-
State - our current state, for example, the current stack. We take it out of the stack and change its state, we get not
only value from the top, but also a new stack.
Or an example with a generator, we want to get 3 random numbers, then we need to pass the generator to the function, get
the first value using it and return this value with the new state of the generator, get the next value random
and the new state of the generator, etc.
An example with a stack but no monads.
-}
type Stack = [Int]

popNotState :: Stack -> (Int,Stack)
popNotState (x:xs) = (x,xs)

pushNotState :: Int -> Stack -> ((),Stack)
pushNotState a xs = ((),a:xs)

stackManipNotState :: Stack -> (Int, Stack)
stackManipNotState stack = let
    ((),newStack1) = pushNotState 3 stack
    (a ,newStack2) = popNotState newStack1
    in popNotState newStack2

{- Not real implementation
newtype State s a = State { runState :: s -> (a,s) }
instance Monad (State s) where
    return x = State $ \s -> (x,s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in  g newState
-}

pop:: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((),a:xs)


stackManip :: State Stack Int
stackManip = do
    push 3
    a <- pop
    pop

stackManipS2 :: State Stack Int
stackManipS2 = push 3 >>=
            \a -> pop >>=
            \_ -> pop

stackManipS3 :: State Stack Int
stackManipS3 = push 3 >>=
            \a -> pop >>=
            \_ -> pop

{-
ghci> runState stackManip [5,8,2,1]
(5,[8,2,1])
-}
stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5
        then push 5
        else do
            push 3
            push 8

stackStuff2 :: State Stack ()
stackStuff2 = pop >>=
  \a ->
    if a==5
      then push 5
      else push 3 >>=
        \_ -> push 8

{-
Remember, do expressions result in monadic values and with the State monad, a single do expression is also a stateful
function. Because stackManip and stackStuff are ordinary stateful computations, we can glue them together to produce
further stateful computations.
-}
moreStack :: State Stack ()
moreStack = do
    a <- stackManip
    if a == 100
        then stackStuff
        else return () -- keeps the state as it is and does nothing.

{-
two useful functions

get = State $ \s -> (s,s) -- present current state as result
put newState = State $ \s -> ((),newState) -- replace state by passed
-}

stackyStack :: State Stack ()  
stackyStack = do  
    stackNow <- get  
    if stackNow == [1,2,3]  
        then put [8,3,1]  
        else put [9,2,1] 
