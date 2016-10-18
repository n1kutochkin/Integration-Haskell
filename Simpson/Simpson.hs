module Simpson.Simpson
  ( Interval (..)
  , getSize
  , getLength
  , points
  , multipliers
  , values
  , integrate )
where

import qualified Data.List()
import qualified System.IO()

-- Compute the division of two Integers as Double
divide :: Int -> Int -> Double
divide a b = fromIntegral a / fromIntegral b

-- generate a list of cycling 4s and 2s with length n
fourTwos :: Int -> [Double]
fourTwos n
  | n < 3 = []
  | otherwise = take (n-2) (cycle [4.0, 2.0])

-- Name general functions as Func
type Func = Double -> Double

--Interval data type and its related functions getSize and getLength
data Interval = Interval (Double, Double) Int
  deriving (Eq, Show)

-- Get Interval size
getSize :: Interval -> Int
getSize (Interval (_, _) n) = n

-- Get Interval length
getLength :: Interval -> Double
getLength (Interval (a,b) _) = b - a

-- Get points of discrete interval
points :: Interval -> [Double]
points (Interval (a, b) n) = [a + (b-a) * divide m n | m <- [0..(n+1)]]

-- Create Simpson method multipliers relative to given Interval
multipliers :: Interval -> [Double]
multipliers (Interval (_, _) n) = 1.0 : fourTwos n ++ [1.0]

-- Compute the function values on given Interval
values :: Func -> Interval -> [Double]
values f i = map f (points i)

-- Integrate function over given interval
integrate :: Func -> Interval -> Double
integrate f i = c * sum (zipWith (*) (multipliers i) (values f i))
  where c = getLength i / fromIntegral (3 * getSize i)
