{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Task1.Benchmark as T1 (runBenchmark)
import Task2.Benchmark as T2 (runBenchmark)
import Task3.Benchmark as T3 (runBenchmark)
main :: IO ()
main = do
  T1.runBenchmark 
