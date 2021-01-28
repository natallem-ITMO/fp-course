{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task8.Grid
  ( Grid (..),
    safeDown,
    safeLeft,
    safeRight,
    safeUp,
  )
where

import Control.Comonad (Comonad (duplicate, extract))
import Data.List (intercalate)
import Data.List.Zipper
  ( Zipper (..),
    beginp,
    cursor,
    extractz,
    left,
    replace,
    right,
  )

-- | Representation of grid comonad
newtype Grid a = Grid {unGrid :: Zipper (Zipper a)}

-- | Focus grid on upper or lower element from current
up, down :: Grid a -> Grid a
up (Grid a) = Grid (left a)
down (Grid a) = Grid (right a)

-- | Focus grid on left or right element from current
gridLeft, gridRight :: Grid a -> Grid a
gridLeft (Grid a) = Grid (fmap left a)
gridRight (Grid a) = Grid (fmap right a)

-- | Check if current focused element is not last in Zipper
isEnd :: Zipper a -> Bool
isEnd (Zip _ [a]) = True
isEnd _ = False

-- | Tries to focus grid on upper or lower element from current
--   if current element in grid is not uppermost or lowermost respectively
safeUp, safeDown :: Grid a -> Maybe (Grid a)
safeUp (Grid a) =
  if beginp a
    then Nothing
    else Just (Grid $ left a)
safeDown (Grid a) =
  if isEnd a
    then Nothing
    else Just (Grid $ right a)

-- | Tries to focus grid on left or right element from current
--   if current element in grid is not leftmost or rightmost respectively
safeLeft, safeRight :: Grid a -> Maybe (Grid a)
safeLeft grid@(Grid zipper) =
  if beginp (cursor zipper)
    then Nothing
    else Just (gridLeft grid)
safeRight grid@(Grid zipper) =
  if isEnd (cursor zipper)
    then Nothing
    else Just (gridRight grid)

-- | Return element in focus in grid
gridRead :: Grid a -> a
gridRead (Grid g) = extractz $ extractz g

-- | Change element in focus in grid by given
gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ replace newLine g
  where
    oldLine = extractz g
    newLine = replace x oldLine

iterateTailN :: Int -> (a -> a) -> a -> [a]
iterateTailN n f a = take n $ tail $ iterate f a

mkZipper :: (v -> v) -> (v -> v) -> v -> Int -> Int -> Zipper v
mkZipper genLeft genRight e leftNum rightNum =
  Zip
    (iterateTailN leftNum genLeft e)
    (e : iterateTailN rightNum genRight e)

horizontal, vertical :: Grid a -> Zipper (Grid a)
vertical grid@(Grid (Zip a (b : c))) = mkZipper up down grid (length a) (length c)
horizontal grid = mkZipper gridLeft gridRight grid (length a) (length c)
  where
    (Zip a (b : c)) = cursor (unGrid grid)

instance Functor Grid where
  fmap :: (a -> b) -> Grid a -> Grid b
  fmap f (Grid zipper) = Grid $ fmap (fmap f) zipper

instance Comonad Grid where
  extract = gridRead
  duplicate = Grid . fmap horizontal . vertical

-- | Instance for show.
--   Print grid for covid simulation in way :
-- |   |   |   |   |   |   |   |
-- |   |   | $ |   |   |   |   |
-- |   |   | # | $ |   |   |   |
-- |   | # | @ | $ |   |   |   |
-- |   | $ | # | $ |   |   |   |
-- |   |   | $ |   |   |   |   |
-- |   |   |   |   |   |   |   |
--  where # - sick person
--        $ - person in incubation period
--        @ - person with immunity
--       ' '- healthy person
instance (Show a) => Show (Grid a) where
  show (Grid (Zip a b)) = unlines $ showLines (reverse a) ++ showLines b
    where
      showLines :: [Zipper a] -> [String]
      showLines = fmap oneLineShow

      oneLineShow :: Zipper a -> String
      oneLineShow (Zip a d) = "| " ++ intercalate " | " (show <$> (reverse a ++ d)) ++ " |"
