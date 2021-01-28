module Block3.Task2
  (   
    element
  , eof
  , ok
  , satisfy
  , stream
  ) where

import Block3.Task1 (Parser (..))

-- | Never fails and never consumes input
ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

-- | Check if parser reached end of input otherwise fails
eof :: Parser s ()
eof = Parser $ \str ->
  case str of
    [] -> Just ((), str)
    _  -> Nothing

-- | Get predicate for element of stream, check if current stream
-- is satisfy predicate and return it otherwise fail
satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser f
  where
    f [] = Nothing
    f (x : xs)
      | p x = return (x, xs)
      | otherwise = Nothing

-- | Check if current stream contains give element and return
--  it otherwise fails
element :: (Eq s) => s -> Parser s s
element c = satisfy (== c)

-- | Check if current stream contains given part of stream and return
--  it otherwise fails
stream :: (Eq s) => [s] -> Parser s [s]
stream = traverse element
