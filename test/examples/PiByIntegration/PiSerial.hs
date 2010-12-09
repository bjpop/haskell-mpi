module Main where

import Control.Applicative ((<$>))
import System (getArgs)

main :: IO ()
main = do
   n <- read <$> head <$> getArgs
   print $ integrate n (1 / fromIntegral n)

integrate :: Int -> Double -> Double
integrate n h =
   h * (sum (map area [1..n]) :: Double)
   -- h * (sum (map area [1..n]))
   where
   area :: Int -> Double
   area i
      = 4 / (1 + x*x)
      where
      x = h * (fromIntegral i - 0.5)
