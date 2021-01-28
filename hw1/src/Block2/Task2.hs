module Block2.Task2
  (
    splitOn
  , joinWith
  ) where

import Data.List.NonEmpty (NonEmpty (..), insert, head, tail)
import Prelude hiding (head, tail)

-- | Split given list of char by given Char separator.
-- If given string is empty, returns empty string. If
-- string starts(or ends) with separator, return
-- head(or tail) empty string in result list of strings.
splitOn :: Char -> [Char] -> NonEmpty [Char]
splitOn splitElem = foldr (foldrHelpFunction splitElem) ("" :| [])
  where 
    foldrHelpFunction separator curChar result
      | curChar == separator = insert "" result
      | otherwise = (curChar : head result) :| tail result

-- | Join by given Char not empty list of Strings.
-- Insert given char between every element of list
joinWith :: Char -> NonEmpty [Char] -> [Char]
joinWith splitElem list = foldl (foldlHelpFunction splitElem) (head list) (tail list)
  where 
    foldlHelpFunction ch acc cur =  acc ++ (ch : cur)
