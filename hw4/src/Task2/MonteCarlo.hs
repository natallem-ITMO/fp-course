module Task2.MonteCarlo (parallelF, consistentF) where

import Control.Monad (forM)
import Control.Monad.Par (InclusiveRange (InclusiveRange), parMapReduceRange, runPar)
import Control.Parallel ()
import Control.Parallel.Strategies (Eval, rpar, runEval)
import System.Random (mkStdGen, randomR)

-- | Parallel realization of Monte Carlo algorithm
parallelF ::
  Int ->                -- ^ number of points on witch function f is calculated
                        --   and summarizes results in one spark
  Int ->                -- ^ number of sparked calculation
  (Double -> Double) -> -- ^ function f to calculate integral of on [a,b]
  Double ->             -- ^ a - left edge of border
  Double ->             -- ^ b - right edge of border 
                        --   (must be less then a for correct work)
  Double                -- ^ result of Monte Carlo algorithm
parallelF
  numberOfElementsInOneThread
  numberOfThreads
  f
  a
  b =
    runEval parallelPart
      / fromIntegral (numberOfElementsInOneThread * numberOfThreads)
        * (b - a)
    where
      parallelPart :: Eval Double
      parallelPart = do
        mass <-
          forM
            [1 .. numberOfThreads]
            ( \numThread ->
                rpar $
                  sum $
                    map
                      (f . fst . randomR (a, b) . mkStdGen)
                      [ numberOfElementsInOneThread * numThread
                        .. numberOfElementsInOneThread * (numThread + 1)
                      ]
            )
        return $ sum mass

-- | Consistent realization of Monte Carlo algorithm
consistentF
  :: Int ->             -- | Number of points for algorithm
  (Double -> Double) -> -- ^ function f on which to calculate the integral on[a,b]
  Double ->             -- ^ a - left edge of border
  Double ->             -- ^ b - right edge of border 
                        --   (must be less then a for correct work)
  Double                -- ^ result of Monte Carlo algorithm
consistentF n f a b = sum array / fromIntegral n * (b - a)
  where
    array :: [Double]
    array = map (f . fst . randomR (a, b) . mkStdGen) [1 .. n]
