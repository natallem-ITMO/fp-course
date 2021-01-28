{-# LANGUAGE InstanceSigs #-}

module Learnyouahaskell.EitherMonad where

import Control.Monad.Error

{-
instance (Error e) => Monad (Either e) where
    return x = Right x
    Right x >>= f = f x
    Left err >>= f = Left err
    fail msg = Left (strMsg msg)
-}

{-
The Monad instance for Either e makes an additional requirement, and that is that the type of the value contained in a
Left, the one that's indexed by the e type parameter, has to be an instance of the Error type class. The Error type
class is for types whose values can act like error messages. It defines the strMsg function, which takes an error in the
form of a string and returns such a value.
-}

{-
ghci> Left "boom" >>= \x -> return (x+1)
Left "boom"
ghci> Right 100 >>= \x -> Left "no way!"
Left "no way!"
-}

{- Possible error while compiling

ghci> Right 3 >>= \x -> return (x + 100)  
<interactive>:1:0:  
    Ambiguous type variable `a' in the constraints:  
      `Error a' arising from a use of `it' at <interactive>:1:0-33  
      `Show a' arising from a use of `print' at <interactive>:1:0-33  
    Probable fix: add a type signature that fixes these type variable(s)  
    
Do not know what use for error type. Simply add:

ghci> Right 3 >>= \x -> return (x + 100) :: Either String Int  
Right 103  

-}