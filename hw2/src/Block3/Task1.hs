{-# LANGUAGE InstanceSigs #-}

module Block3.Task1
  ( 
    Parser (..)
  ) where

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
