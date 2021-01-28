module Task3.Benchmark (runBenchmark) where

import Control.Monad.Extra (forM, forM_)
import Criterion.Main (bench, bgroup, defaultMain, env, nf, whnf)
import System.IO.Unsafe (unsafePerformIO)
import Task3.ConcurrentHashTable
  ( getCHT,
    newCHT,
    putCHT,
    sizeCHT,
  )

runBenchmark :: IO ()
runBenchmark =
  defaultMain
    [ bgroup
        "only add"
        (map (\x -> bench (show x) $ whnf addToHashMap x) allRuns),
      bgroup
        "add and get"
        (map (\x -> bench (show x) $ nf addAndGetBackResult x) allRuns)
    ]

maxNumberElements :: [Int]
maxNumberElements = [100, 1000, 10000]

addAndGetBackResult :: Int -> [Maybe Int]
addAndGetBackResult num =
  unsafePerformIO
    ( do
        table <- newCHT
        forM_ [1 .. num] (\x -> putCHT (show x) x table)
        forM
          [1 .. num]
          (flip getCHT table . show)
    )

addToHashMap :: Int -> Int
addToHashMap num =
  unsafePerformIO
    ( do
        table <- newCHT
        forM_ [1 .. num] (\x -> putCHT (show x) x table)
        sizeCHT table
    )

allRuns :: [Int]
allRuns = [10, 100, 1000, 10000]

-- Results:

-- benchmarking only add/10
-- time                 1.735 μs   (1.723 μs .. 1.745 μs)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 1.736 μs   (1.728 μs .. 1.743 μs)
-- std dev              25.92 ns   (20.48 ns .. 31.41 ns)
-- variance introduced by outliers: 14% (moderately inflated)

-- benchmarking only add/100
-- time                 65.33 μs   (65.02 μs .. 65.63 μs)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 65.44 μs   (65.25 μs .. 65.62 μs)
-- std dev              610.4 ns   (498.7 ns .. 740.5 ns)

-- benchmarking only add/1000
-- time                 1.645 ms   (1.642 ms .. 1.649 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 1.652 ms   (1.649 ms .. 1.657 ms)
-- std dev              14.18 μs   (10.90 μs .. 20.22 μs)

-- benchmarking only add/10000
-- time                 92.16 ms   (87.51 ms .. 98.55 ms)
--                      0.991 R²   (0.969 R² .. 1.000 R²)
-- mean                 91.52 ms   (89.17 ms .. 95.00 ms)
-- std dev              4.617 ms   (1.774 ms .. 5.886 ms)
-- variance introduced by outliers: 10% (moderately inflated)

-- benchmarking add and get/10
-- time                 2.781 μs   (2.763 μs .. 2.806 μs)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 2.786 μs   (2.771 μs .. 2.800 μs)
-- std dev              55.35 ns   (46.49 ns .. 68.55 ns)
-- variance introduced by outliers: 22% (moderately inflated)

-- benchmarking add and get/100
-- time                 79.19 μs   (78.35 μs .. 80.45 μs)
--                      0.990 R²   (0.973 R² .. 0.997 R²)
-- mean                 89.25 μs   (85.39 μs .. 99.39 μs)
-- std dev              21.42 μs   (10.53 μs .. 40.40 μs)
-- variance introduced by outliers: 96% (severely inflated)

-- benchmarking add and get/1000
-- time                 1.904 ms   (1.875 ms .. 1.944 ms)
--                      0.995 R²   (0.990 R² .. 0.998 R²)
-- mean                 1.886 ms   (1.862 ms .. 1.924 ms)
-- std dev              91.00 μs   (55.84 μs .. 123.3 μs)
-- variance introduced by outliers: 33% (moderately inflated)

-- benchmarking add and get/10000
-- time                 101.1 ms   (95.92 ms .. 105.2 ms)
--                      0.993 R²   (0.970 R² .. 0.999 R²)
-- mean                 99.67 ms   (97.02 ms .. 103.9 ms)
-- std dev              5.500 ms   (3.146 ms .. 8.266 ms)
-- variance introduced by outliers: 10% (moderately inflated)
