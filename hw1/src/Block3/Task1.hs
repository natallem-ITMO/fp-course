module Block3.Task1
  (
     eitherConcat
  ,  maybeConcat
  )  where

import Data.Bifunctor (bimap)
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes)

-- | Accepts the list of lists
-- inside Maybe and returns the
-- concatenation of all internal
-- lists for Just
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = concat . catMaybes

-- | Return pair of results
--  of the monoidal combination of 
-- the separate elements inside Left
--  and the separate elements inside Right
eitherConcat :: (Monoid a, Monoid b) 
  => [Either a b] -> (a , b)
eitherConcat list = 
  bimap mconcat mconcat partList
    where 
      partList = partitionEithers list
