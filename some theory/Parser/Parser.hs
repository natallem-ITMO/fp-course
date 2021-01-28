{-# LANGUAGE InstanceSigs #-}
module Parser where

import Control.Applicative (Alternative (..))

-- | Representation of Parser for any type of stream,
-- that can be used in combinator parser
newtype Parser s a = Parser {runParser :: [s] -> Maybe (a, [s])}

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser runP) = Parser $ \str ->
    do
      (result, rest) <- runP str
      return (f result, rest)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure x = Parser $ \str -> pure (x, str)

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (Parser runFunc) <*> (Parser runArg) = Parser $ \str ->
    do
      (x, y) <- runFunc str
      (x2, y2) <- runArg y
      return (x x2, y2)

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ const Nothing

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (<|>) (Parser runP1) (Parser runP2) = Parser $ \str -> runP1 str <|> runP2 str

instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  (Parser arg1) >>= func = Parser $ \str ->
    do
      (res, ost) <- arg1 str
      let (Parser kk) = func res
      kk ost

-- | MonadFail instance used in Block3.Task4
instance MonadFail (Parser s) where
  fail _ = Parser $ const Nothing

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

type StrParser = Parser Char

-- | Representation of data for CBS
--   EmptyCBS - empty CBS
--   InBrackets - (CBS)CBS*
data CBS
  = EmptyCBS ()
  | InBrackets CBS CBS

-- | Instance uses in tests
instance Show CBS where
  show (EmptyCBS ()) = ""
  show (InBrackets x y) = "(" ++ show x ++ ")" ++ show y

-- | Parser check if all given string is CBF, return CBS data type if so,
-- otherwise fails
correctBracketSequence :: StrParser CBS
correctBracketSequence = oneCBS <* eof
  where
    oneCBS = (InBrackets <$> inBrackets <*> oneCBS) <|> (EmptyCBS <$> ok)
    inBrackets = (\_ x _ -> x) <$> element '(' <*> oneCBS <*> element ')'

-- | Parsing integer that can be represented as <+"num"> or <-"num"> or <"num">
--  where "num" is sequence of digits characters. If stream contains such number,
-- return it and rest of stream, otherwise fails
integerParser :: StrParser Integer
integerParser =
  signConstructor
    <$> (element '+' <|> element '-' <|> return 'x')
    <*> simpleIntegerParser
  where
  
    signConstructor :: Char -> Integer -> Integer
    signConstructor '-' num = negate num
    signConstructor _ num = num

    simpleIntegerParser = Parser f
      where
        f xs
          | null ns = Nothing
          | otherwise = Just (read ns, rest)
          where
            (ns, rest) = span isDigit xs

-- | Parser for zero of more number of space character
spaces :: StrParser String
spaces = many (satisfy isSpace)

-- | Parser of a list of lists of numbers separated by commas. All
--   numbers are listed separated by commas. At the beginning of each
--  list is a number --- the length of the list. This way, you can
--  understand where each list ends.
--  If all input string does not represents correct list of lists,
-- parsing is failed.
listlistParser :: StrParser [[Int]]
listlistParser = ((:) <$> listParser <*> many commaListParser) <* spaces <* eof

-- | Parsing list of numbers, that goes after comma,
-- surrounded by any nymber of spaces
commaListParser :: StrParser [Int]
commaListParser = do
  _ <- betweenSpace (element ',')
  listParser

-- | Use some parser, skipping previous and subsequent spaces
betweenSpace :: StrParser s -> StrParser s
betweenSpace parser = do
  _ <- spaces
  x <- parser
  _ <- spaces
  return x

-- | Parsing one list, first parse number N - length of
-- numbers in list, further trys to parse all N number of
-- array, otherwise fails
listParser :: StrParser [Int]
listParser = do
  x <- betweenSpace integerParser
  when (x < 0) $ fail "negative length"
  readNNumbers x

-- | parsing exactly N numbers of input stream, separeted by commas
readNNumbers :: Integer -> StrParser [Int]
readNNumbers 0 = return []
readNNumbers n = do
  _ <- betweenSpace (element ',')
  x <- integerParser
  rest <- readNNumbers (n - 1)
  return (fromIntegral x : rest)



-- combinators
-- * Functor
-- fmap  :: (a -> b) -> Parser a -> Parser b
-- (<$)  :: a -> Parser b -> Parser a



-- -- * Applicative
--p1 <*> p2 represents the parser which first runs p1 (which will
-- consume some input and produce a function), then passes the
-- remaining input to p2 (which consumes more input and produces
-- some value), then returns the result of applying the function to the
-- value. However, if either p1 or p2 fails then the whole thing should
-- also fail (put another way, p1 <*> p2 only succeeds if both p1 and
-- p2 succeed)
-- pure  :: a -> Parser a
-- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
-- (<*)  :: Parser a -> Parser b -> Parser a -- run both in sequence, result of first
-- (*>)  :: Parser a -> Parser b -> Parser b -- similar to above


-- -- * Alternative
-- p1 <|> p2 represents the parser which first tries running p1. If
-- p1 succeeds then p2 is ignored and the result of p1 is returned.
-- Otherwise, if p1 fails, then p2 is tried instead.
-- empty :: Parser a
-- (<|>) :: Parser a -> Parser a -> Parser a -- orElse
-- many  :: Parser a -> Parser [a] -- zero or more
-- some  :: Parser a -> Parser [a] -- one or more (should be NonEmpty) 

-- -- * Monadic
-- (>>=) :: Parser a -> (a -> Parser b) -> Parser b  -- andThen




instance Functor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap f (Just x) = Just (f x)
    fmap _ Nothing  = Nothing


instance Functor [] where
    fmap :: (a -> b) -> [a] -> [b]
    fmap = map
instance Functor ((->) r)  where
    fmap :: (a -> b) -> (r -> a) -> r -> b
    fmap = (.)

instance Applicative Maybe where
    pure :: a -> Maybe a
    pure = Just
    
    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    Nothing <*> _         = Nothing
    Just f  <*> something = fmap f something
instance Applicative [] where
    pure :: a -> [a]
    pure x    = [x]

    (<*>) :: [a -> b] -> [a] -> [b]
    fs <*> xs = [f x | f <- fs, x <- xs]

instance Applicative ((->) r) where
    pure :: a -> r -> a
    pure x  = \_ -> x

    (<*>) :: (r -> a -> b) -> (r -> a) -> r -> b
    f <*> g = \x -> f x (g x)

instance Alternative Maybe where
    empty :: Maybe a
    empty = Nothing

    (<|>) :: Maybe a -> Maybe a -> Maybe a
    Nothing <|> r = r
    l       <|> _ = l

instance Alternative [] where
    empty :: [a]
    empty = []
    
    (<|>) :: [a] -> [a] -> [a]
    (<|>) = (++)


instance Traversable Maybe where
    traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
    traverse _ Nothing  = pure Nothing
    traverse f (Just x) = Just <$> f x

instance Traversable [] where
    traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
    traverse f = foldr consF (pure [])
      where 
        consF x ys = (:) <$> f x <*> ys
