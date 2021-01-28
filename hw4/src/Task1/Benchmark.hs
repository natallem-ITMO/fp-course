module Task1.Benchmark (runBenchmark) where

import Criterion.Main (bench, bgroup, defaultMain, env, nf)

import Task1.Geometry.FastGeometry as Fast (Point (Point), doubleArea, perimeter)

import Task1.Geometry.SlowGeometry as Slow (Point (Point), doubleArea, perimeter)

-- | Function to run benchmark in Main file
--   Test space and time of running function
--   'perimeter' and 'doubleArea' of slow and
--   fast geometry realization.
--
--   Each function applies on auto generated
--   rectangle of size (width x high):
--   1) 200 x 300 - small object (10^3 points)
--   1) 20000 x 30000 - big object (10^5 points)
--   1) 2000000 x 3000000 - very big object (10^7 points)
--
runBenchmark :: IO ()
runBenchmark =
  defaultMain
    [ env setupFastRectangles $ \ ~(small, big, veryBig) ->
        bgroup
          "fast geometry"
          [ bgroup
              "perimeter"
              [ bench "small(10^3)" $ nf Fast.perimeter small,
                bench "big(10^5)" $ nf Fast.perimeter big,
                bench "veryBig(10^7)" $ nf Fast.perimeter veryBig
              ],
            bgroup
              "double area"
              [ bench "small(10^3)" $ nf Fast.doubleArea small,
                bench "big(10^5)" $ nf Fast.doubleArea big,
                bench "veryBig(10^7)" $ nf Fast.doubleArea veryBig
              ]
          ]
      , env setupSlowRectangles $ \ ~(small, big, veryBig) ->
        bgroup
          "slow geometry"
          [ bgroup
              "perimeter"
              [ bench "small(10^3)" $ nf Slow.perimeter small,
                bench "big(10^5)" $ nf Slow.perimeter big,
                bench "veryBig(10^7)" $ nf Slow.perimeter veryBig
              ],
            bgroup
              "double area"
              [ bench "small(10^3)" $ nf Slow.doubleArea small,
                bench "big(10^5)" $ nf Slow.doubleArea big,
                bench "veryBig(10^7)" $ nf Slow.doubleArea veryBig
              ]
          ]
    ]

-- | Setup environment of three rectangles of fast geometry for benchmarking
setupFastRectangles :: IO ([Fast.Point], [Fast.Point], [Fast.Point])
setupFastRectangles = do
  let small = generateFastRectangle 200 300 (Fast.Point 1 1)
  let big = generateFastRectangle 20000 30000 (Fast.Point 1 1)
  let veryBig = generateFastRectangle 2000000 3000000 (Fast.Point 1 1)
  return (small, big, veryBig)

-- | Generate points of rectangle in fast geometry with given width and high.
generateFastRectangle
  :: Int          -- ^ width of rectangle
  -> Int          --   hight of rectangle
  -> Fast.Point   --   Init point of left down corner of rectangle
  -> [Fast.Point] --   Resulting rectangle (points in counterclockwise order)
                  --   Example (A - start point, size of rectangle [7 x 4])
                  -- (0,4).......(7,4)
                  --      .     .
                  --      .     .
                  -- (0,0)A......(7,0)
generateFastRectangle w h (Fast.Point x y) =
  map (\i -> Fast.Point (x + i) y) (take (w + 1) [0 ..])
    ++ map (\i -> Fast.Point (x + w) (y + i)) (take h [1 ..])
    ++ map (\i -> Fast.Point (x + w - i) (y + h)) (take w [1 ..])
    ++ map (\i -> Fast.Point x (y + h - i)) (take (h -1) [1 ..])

-- | Setup environment of three rectangles of slow geometry for benchmarking
setupSlowRectangles :: IO ([Slow.Point], [Slow.Point], [Slow.Point])
setupSlowRectangles = do
  let small = generateSlowRectangle 200 300 (Slow.Point 1 1)
  let big = generateSlowRectangle 20000 30000 (Slow.Point 1 1)
  let veryBig = generateSlowRectangle 2000000 3000000 (Slow.Point 1 1)
  return (small, big, veryBig)

-- | Generate points of rectangle in fast geometry with given width and high.
generateSlowRectangle
  :: Int          -- ^ width of rectangle
  -> Int          --   hight of rectangle
  -> Slow.Point   --   Init point of left down corner of rectangle
  -> [Slow.Point] --   Resulting rectangle (points in counterclockwise order)
                  --   Example (A - start point, size of rectangle [7 x 4])
                  -- (0,4).......(7,4)
                  --      .     .
                  --      .     .
                  -- (0,0)A......(7,0)
generateSlowRectangle w h (Slow.Point x y) =
  map (\i -> Slow.Point (x + i) y) (take (w + 1) [0 ..])
    ++ map (\i -> Slow.Point (x + w) (y + i)) (take h [1 ..])
    ++ map (\i -> Slow.Point (x + w - i) (y + h)) (take w [1 ..])
    ++ map (\i -> Slow.Point x (y + h - i)) (take (h -1) [1 ..])
