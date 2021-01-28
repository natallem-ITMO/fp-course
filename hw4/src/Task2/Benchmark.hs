module Task2.Benchmark (runBenchmark) where

import Criterion.Main (bench, bgroup, defaultMain, env, whnf)

import Task2.MonteCarlo (consistentF, parallelF)

runBenchmark :: IO ()
runBenchmark =
  defaultMain
    [ 
      bgroup
        "10^3"
        [ bench "consistent" $ whnf (consistentF 1000 testF 0.1) 1.0,
          bench "parallel" $ whnf (parallelF 100 10 testF 0.1) 1.0
        ],
      bgroup 
        "10^4"
        [ bench "consistent" $ whnf (consistentF 10000 testF 0.1) 1.0,
          bench "parallel" $ whnf (parallelF 100 100 testF 0.1) 1.0
        ],
      bgroup 
        "10^6"
        [ bench "consistent" $ whnf (consistentF 1000000 testF 0.1) 1.0,
          bench "parallel" $ whnf (parallelF 1000 1000 testF 0.1) 1.0
        ],
      bgroup 
        "10^7"
        [ bench "consistent" $ whnf (consistentF 10000000 testF 0.1) 1.0,
          bench "parallel" $ whnf (parallelF 1000 10000 testF 0.1) 1.0
        ]
    ]

testF :: Double -> Double 
testF x = (1 / tan (x * x)) - cos x

-- RESULTS:
-- benchmarking 10^3/consistent
-- time                 938.6 μs   (915.7 μs .. 963.5 μs)
--                      0.960 R²   (0.918 R² .. 0.986 R²)
-- mean                 1.119 ms   (1.036 ms .. 1.218 ms)
-- std dev              274.7 μs   (205.3 μs .. 349.5 μs)
-- variance introduced by outliers: 95% (severely inflated)

-- benchmarking 10^3/parallel
-- time                 458.1 μs   (445.2 μs .. 480.1 μs)
--                      0.982 R²   (0.970 R² .. 0.991 R²)
-- mean                 463.0 μs   (449.2 μs .. 480.6 μs)
-- std dev              49.33 μs   (36.47 μs .. 63.45 μs)
-- variance introduced by outliers: 79% (severely inflated)

-- benchmarking 10^4/consistent
-- time                 9.684 ms   (9.496 ms .. 9.840 ms)
--                      0.995 R²   (0.987 R² .. 0.999 R²)
-- mean                 10.26 ms   (9.967 ms .. 10.91 ms)
-- std dev              1.102 ms   (476.2 μs .. 2.008 ms)
-- variance introduced by outliers: 58% (severely inflated)

-- benchmarking 10^4/parallel
-- time                 2.192 ms   (2.094 ms .. 2.264 ms)
--                      0.976 R²   (0.957 R² .. 0.988 R²)
-- mean                 2.825 ms   (2.660 ms .. 3.197 ms)
-- std dev              821.8 μs   (508.4 μs .. 1.338 ms)
-- variance introduced by outliers: 96% (severely inflated)

-- benchmarking 10^6/consistent
-- time                 953.3 ms   (950.8 ms .. 958.3 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 952.6 ms   (950.9 ms .. 953.7 ms)
-- std dev              1.700 ms   (665.5 μs .. 2.332 ms)
-- variance introduced by outliers: 19% (moderately inflated)

-- benchmarking 10^6/parallel
-- time                 164.3 ms   (116.7 ms .. 190.2 ms)
--                      0.959 R²   (0.865 R² .. 1.000 R²)
-- mean                 194.3 ms   (178.7 ms .. 214.7 ms)
-- std dev              23.87 ms   (14.89 ms .. 30.43 ms)
-- variance introduced by outliers: 31% (moderately inflated)

-- benchmarking 10^7/consistent
-- time                 11.64 s    (10.19 s .. 12.69 s)
--                      0.998 R²   (0.997 R² .. 1.000 R²)
-- mean                 10.55 s    (10.11 s .. 10.99 s)
-- std dev              542.8 ms   (242.9 ms .. 659.5 ms)
-- variance introduced by outliers: 19% (moderately inflated)

-- benchmarking 10^7/parallel
-- time                 4.126 s    (4.011 s .. 4.220 s)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 4.200 s    (4.159 s .. 4.277 s)
-- std dev              75.28 ms   (4.168 ms .. 89.08 ms)
-- variance introduced by outliers: 19% (moderately inflated)
