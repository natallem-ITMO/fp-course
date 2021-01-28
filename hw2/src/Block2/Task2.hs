module Block2.Task2
  (
    moving
  ) where

import Control.Monad (unless, when)
import Control.Monad.State.Lazy (State, execState, get, put, state)

-- | Representation of state while calculating
-- Simple Moving Average algorithm
-- Store current state of algorithm.
-- Using queue created from by two stack (aka list), to achieve O(1)
-- complexity while moving window by input array.
-- Use word "window" as a term to represent current element,
-- which we account while calculating current average.
data MovingState = MovingState
  { -- | aka number of element to account for average
    maximumWindowLength :: Int
    -- | input array
  , input :: [Double]
    -- | where put element (first element of list is last element of queue)
  , windowInput :: [Double]
    -- | from where get element (first element of list is first element of queue)
  , windowOutput :: [Double]
    -- | number of elements in window
    --  (need for checking if we need to throw away window element)
  , windowLength :: Int
    -- | current sum of elements in window
  , windowSum :: Double
    -- | current reversed result (because of adding in begin of list)
  , result :: [Double]
  }

-- | Get first element from input array, return it and delete it from input array
popInput :: State MovingState Double
popInput = state $ \st -> (head (input st), st {input = tail (input st)})

-- | Add element in the end of queue, consisting of two stacks(lists)
enqueueWindow :: Double -> State MovingState ()
enqueueWindow x = state $ \st ->
  ( (),
    st
      { windowInput = x : windowInput st,
        windowLength = windowLength st + 1,
        windowSum = windowSum st + x
      }
  )

-- | Remove first element to queue consisting of two stacks(lists)
dequeueWindow :: State MovingState ()
dequeueWindow = do
  curMovingState <- get
  when
    (null (windowOutput curMovingState))
    moveFromInputToOutputStack
  removeFromWindowOutput
  where
    removeFromWindowOutput = state $ \st ->
      ( (),
        st
          { windowOutput = tail (windowOutput st),
            windowLength = windowLength st - 1,
            windowSum = windowSum st - head (windowOutput st)
          }
      )

-- | Helper function to support fullness of output stack if it's empty
--   Put all elements of input stack into output stack.
-- Call only if output stack is empty
moveFromInputToOutputStack :: State MovingState ()
moveFromInputToOutputStack = do
  curMovingState <- get
  let curWindowInput = windowInput curMovingState
  unless (null curWindowInput) $
    do
      put
        curMovingState
          { windowInput = tail curWindowInput,
            windowOutput = head curWindowInput : windowOutput curMovingState
          }
      moveFromInputToOutputStack

-- | Add average of element
pushToResult :: Double -> State MovingState ()
pushToResult x = state $ \st ->
  ((), st {result = x : result st})

-- | Simple Moving Average algorithm realization
-- Calculate average accounting N previous elements
-- (N - first argument of function) of input Double array (2nd argument)
--  If N is not positive - throw error
moving :: Int -> [Double] -> [Double]
moving num arr
  | num <= 0 = error "Fist argument cannot be negative"
  | otherwise =
    reverse $
      result $
        execState monadMoving movingStateFromInput
  where
    movingStateFromInput =
      MovingState
        { maximumWindowLength = num,
          input = arr,
          windowInput = [],
          windowOutput = [],
          windowLength = 0,
          windowSum = 0,
          result = []
        }
    monadMoving = do
      initState <- get
      unless (null (input initState)) $
        do
          x <- popInput
          curState <- get
          when
            (windowLength curState == maximumWindowLength curState)
            dequeueWindow
          enqueueWindow x
          newCurState <- get
          pushToResult
            (windowSum newCurState / fromIntegral (windowLength newCurState))
          monadMoving
