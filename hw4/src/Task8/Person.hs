{-# LANGUAGE ScopedTypeVariables #-}

module Task8.Person
  ( Person (..),
    curIllness,
    curImmunity,
    curIncubation,
    g,
    isIncubation,
    isImmunity,
    isSick,
  )
where

import Lens.Micro ( (^.), lens, Lens' )
import System.Random ( StdGen ) 

-- | Data representation of person in cell in covid emulation grid
--   If person sick, then _curIncubation > 0
--   If person has immunity, then _curImmunity > 0
--   If person is in incubation period, then _curIncubation > 0
--   If person is healthy, then all current fields = 0
data Person = Person
  { prob :: Double, 
    -- ^ Probability to catch covid from sick or incubator neighbour 
    incubation :: Int,
    -- ^ Incubation period in days for current covid emulation
    illness :: Int,
    -- ^ Illness period in days for current covid emulation
    immunity :: Int,
    -- ^ Immunity period in days for current covid emulation
    _g :: StdGen,
    -- ^ Generator for person to calculate probability to catch covid 
    _curIncubation :: Int,
    -- ^ Number of days, that left to be in incubation period for person
    --   0 if person is not in incubation period
    _curIllness :: Int,
    -- ^ Number of days, that left to be ill for person
    --   0 if person is not ill
    _curImmunity :: Int
    -- ^ Number of days, that left to have immunity for person
    --   0 if person doesn't have immunity
  }

-- | Show instance to print covid grid 
instance Show Person where
  show person
    | isSick person = "#"
    | isIncubation person = "$"
    | isImmunity person = "@"
    | otherwise = " "

-- | Lens for _g field (generator)
g :: Lens' Person StdGen
g = lens _g (\p a -> p {_g = a})

-- | Lens for _curIncubation field
curIncubation :: Lens' Person Int
curIncubation = lens _curIncubation (\p a -> p {_curIncubation = a})

-- | Lens for _curIllness field
curIllness :: Lens' Person Int
curIllness = lens _curIllness (\p a -> p {_curIllness = a})

-- | Lens for _curImmunity field
curImmunity :: Lens' Person Int
curImmunity = lens _curImmunity (\p a -> p {_curImmunity = a})

-- | Checks if person has immunity
isImmunity :: Person -> Bool
isImmunity person = person ^. curImmunity > 0

-- | Checks if person is sick
isSick :: Person -> Bool
isSick person = person ^. curIllness > 0

-- | Checks if person is in incubation period
isIncubation :: Person -> Bool
isIncubation person = person ^. curIncubation > 0
