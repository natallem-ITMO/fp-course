module Block1.Task1
  (  Day (..)
  ,  afterDays
  ,  daysToParty
  ,  isWeekend
  ,  nextDay
  )  where

import Data.Maybe (fromJust, fromMaybe)
import Data.Tuple (swap)

-- | Days of week representation used in task.
data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

instance Enum Day where
  fromEnum = fromJust . flip lookup dayToIntTable
  toEnum x =
    fromMaybe
      ( error
          ( "toEnum{Day}: tag ("
              ++ show x
              ++ ") is outside of enumeration's range (0,6)"
          )
      )
      (lookup x (map swap dayToIntTable))
  succ Sunday = Monday
  succ x = toEnum $ (+) 1 $ fromEnum x

instance Eq Day where
  Monday == Monday = True
  Tuesday == Tuesday = True
  Wednesday == Wednesday = True
  Thursday == Thursday = True
  Friday == Friday = True
  Saturday == Saturday = True
  Sunday == Sunday = True
  _ == _ = False
  
-- | Day to Int table using in instance Enum for Day data
dayToIntTable :: [(Day, Int)]
dayToIntTable =
  [ (Monday, 0)
  ,  (Tuesday, 1)
  ,  (Wednesday, 2)
  ,  (Thursday, 3)
  ,  (Friday, 4)
  ,  (Saturday, 5)
  ,  (Sunday, 6)
  ]
  
-- | Return next Day for argument day
nextDay :: Day -> Day
nextDay = succ

-- | Return the day, after n days after argument day. Possible negative n
afterDays :: Day -> Int -> Day
afterDays day n = toEnum $ mod (fromEnum day + n) 7

-- | Return True  if argument day is weekend, else False
isWeekend :: Day -> Bool
isWeekend Sunday = True
isWeekend Saturday = True
isWeekend _ = False

-- | Return number of days until friday(excluded), starting from argument day(included)
daysToParty :: Day -> Int
daysToParty day = (4 + (7 - x)) `mod` 7
  where
    x = fromEnum day
