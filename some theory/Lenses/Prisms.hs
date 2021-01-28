module Lenses.Prisms
where
import Control.Lens.Combinators
import  Control.Lens.Operators
import Data.Maybe

exEither :: Maybe Int 
exEither = preview _Left (Left 2) -- return if constructor

exEither2 :: Either Integer c
exEither2 = review _Left 5 -- construct by element

exEither3 :: Maybe String 
exEither3 = Left "Df" ^? _Left -- == preview 

type A = Either String (Maybe (Int, Double))

f :: A -> Maybe Double
f a = preview  (_Right._Just._2) a
