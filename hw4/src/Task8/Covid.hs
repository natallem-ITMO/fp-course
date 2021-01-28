module Task8.Covid where

import Control.Comonad (Comonad (extend, extract))
import Data.List (intercalate)
import Data.List.Zipper (fromList)
import Data.Maybe (mapMaybe)
import Lens.Micro ((%~), (&), (.~), (^.))
import System.Random (StdGen, mkStdGen, randomR)

import Task8.Grid
  ( Grid (..),
    safeDown,
    safeLeft,
    safeRight,
    safeUp,
  )
import Task8.Person
  ( Person (..),
    curIllness,
    curImmunity,
    curIncubation,
    g,
    isImmunity,
    isIncubation,
    isSick,
  )

-- | Create Grid of Person for Covid spread emulation
--   Returns Grid where in the middle one sick person,
--     others are healthy
mkGrid ::
  Int -> --  Size of grid (n x n),
         --      should be not even
  Double -> --  Probability to catch infection
  Int -> --  Number of days for incubation period
  Int -> --  Number of days for illness period,
         --      should be > 0, otherwise undefined behavior
  Int -> --  Number of days then person have immunity
  Grid Person
mkGrid size pr incub ill imm
  | even size = error "grid cannot be even size"
  | otherwise =
    Grid $
      fromList $
        map
          ( \y ->
              fromList $
                map
                  ( \x ->
                      Person
                        { prob = pr,
                          incubation = incub,
                          illness = ill,
                          immunity = imm,
                          _g = mkStdGen $ (size * y) + x,
                          _curIncubation = 0,
                          _curIllness = ifMiddle size y x ill,
                          _curImmunity = 0
                        }
                  )
                  [1 .. size]
          )
          [1 .. size]
  where
    ifMiddle :: Int -> Int -> Int -> Int -> Int
    ifMiddle size line column ill =
      let halfSize = (size + 1) `div` 2
       in if line == halfSize && column == halfSize then ill else 0

getNeighbours :: [Grid a -> Maybe (Grid a)]
getNeighbours = [safeRight, safeLeft, safeUp, safeDown]

-- | Calculate probability if person in focused grid
--   catch covid or not and return True, if so,
becomeSick ::
  Grid Person ->  -- Grid with person in focus
  Double ->       -- Threshold, if any probability to catch covid from
                  -- neighbours is higher then threshold, the person becomes sick
  Bool
becomeSick grid threshold = any (< threshold) probabilities
  where
    neighbours :: [Person]
    neighbours =
      map extract $
        mapMaybe (\direction -> direction grid) getNeighbours
    probabilities :: [Double]
    probabilities =
      map
        (fst . randomR (0.0, 1.0) . combineRandomGen)
        (filter (\p -> isSick p || isIncubation p) neighbours)
    combineRandomGen :: Person -> StdGen
    combineRandomGen person =
      mkStdGen
        ( fst (randomR (1 :: Int, 1000) (person ^. g))
            + fst (randomR (1 :: Int, 5000) (extract grid ^. g))
        )

-- | Rule of simulation for each person
--   Change condition of health for person
rule :: Grid Person -> Person
rule grid = refreshGen $ conditions person
  where
    person = extract grid
    conditions person
      | isSick person =
        let afterSick = person & curIllness %~ pred
         in if not (isSick afterSick)
              then afterSick & curImmunity .~ immunity afterSick
              else afterSick
      | isIncubation person =
        let afterIncubation = person & curIncubation %~ pred
         in if not (isIncubation afterIncubation)
              then afterIncubation & curIllness .~ illness afterIncubation
              else afterIncubation
      | isImmunity person = person & curImmunity %~ pred
      | otherwise =
        if becomeSick grid (prob person)
          then
            let afterIncubation = person & curIncubation .~ incubation person
             in if not (isIncubation afterIncubation)
                  then afterIncubation & curIllness .~ illness afterIncubation
                  else afterIncubation
          else person

    refreshGen :: Person -> Person -- Renew generator in each person for every step
    refreshGen person = person & g .~ snd (randomR (0.0 :: Double, 1.0) (person ^. g))

-- | Get next day of people population in grid
nextStep :: Grid Person -> Grid Person
nextStep = extend rule

-- To print covid grid, use Show instance of Grid
