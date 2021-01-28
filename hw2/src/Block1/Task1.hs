module Block1.Task1
  ( 
    stringSum
  ) where

-- | Find sum of all divided by space characters numbers, if all numbers is
-- correct
stringSum :: String -> Maybe Int
stringSum str = sum <$> traverse readMaybeInt (words str)
  where
    readMaybeInt s = case reads s of
      [(val, "")] -> Just val
      _           -> Nothing
