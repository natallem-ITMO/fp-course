{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}

module Task1.Geometry.FastGeometry
  ( Point (..),
    crossProduct,
    doubleArea,
    minus,
    perimeter,
    plus,
    scalarProduct,
  )
where

import Control.DeepSeq (NFData (..), ($!!))
import Data.List (foldl')

-- |  Data representing Point structure of two coordinates
data Point = Point {x :: !Int, y :: !Int}
  deriving (Show)

-- | Used in Benchmark
instance NFData Point where
  rnf :: Point -> ()
  rnf (Point a b)  = rnf a `seq` rnf b

-- | Adds two points by summarizing their coordinates
plus
  :: Point -- ^ First point with coordinates (x1,y1)
  -> Point -- ^ Second point with coordinates (x2,y2)
  -> Point -- ^ Result point with coordinates (x1+x2,y1+y2)
plus a b = Point {x = x a + x b, y = y a + y b}

-- | Returns a point that is the difference of two points
minus
  :: Point -- ^ First point with coordinates (x1,y1)
  -> Point -- ^ Second point with coordinates (x2,y2)
  -> Point -- ^ Result point with coordinates (x1-x2,y1-y2)
minus a b = Point {x = x a - x b, y = y a - y b}

-- | Return a point that is the scalar product of two points
scalarProduct
  :: Point -- ^ First point with coordinates (x1,y1)
  -> Point -- ^ Second point with coordinates (x2,y2)
  -> Int   -- ^ Scalar product = x1 * x2 + y1 * y2
scalarProduct !a !b = x a * x b + y a * y b

-- | Return a point that is the scalar product of two points
crossProduct
  :: Point -- ^ First point with coordinates (x1,y1)
  -> Point -- ^ Second point with coordinates (x2,y2)
  -> Int   -- ^ Scalar product = x1 * y2 - x2 * y1
crossProduct !a !b = x a * y b - x b * y a

-- | Return perimeter of given polygon without self-intersection
perimeter
  :: [Point] -- ^ Coordinates of input polygon vertices
             --   in counterclockwise order
  -> Double  -- ^ Perimeter of given polygon
perimeter  = traversePolygonWithFunction calcDistance
  where
    calcDistance :: Point -> Point -> Double
    calcDistance p1 p2 = let !diffPoint = minus p1 p2
      in sqrt $! fromIntegral $! scalarProduct diffPoint diffPoint

-- | Return double area of given polygon without self-intersection
doubleArea
  :: [Point] -- ^ Coordinates of input polygon vertices
             --   in counterclockwise order
  -> Double  -- ^ double area of given polygon
doubleArea = traversePolygonWithFunction calcTwoPointArea
  where
    calcTwoPointArea :: Point -> Point -> Double
    calcTwoPointArea p1 p2 = fromIntegral $! crossProduct p1 p2

-- | Traverse points of polygon, calculating
--   given function on each of its edges in counterclockwise order
traversePolygonWithFunction
  :: (Point -> Point -> Double) -- ^ function to apply on each adjust vertices
  -> [Point]                    -- ^ polygon vertices
  -> Double                     -- ^ result of traverse
traversePolygonWithFunction _ [] = error "Not a polygon"
traversePolygonWithFunction func points =
  fst convolutionWithoutLast
  +
  func (snd convolutionWithoutLast) (head points)
  where
    convolutionWithoutLast :: (Double, Point)
    !convolutionWithoutLast = foldl' funcFold (0.0, head points) (tail points)

    funcFold :: (Double, Point) -> Point -> (Double, Point)
    funcFold (!accValue, prevPoint) point = (accValue + func prevPoint point, point)
