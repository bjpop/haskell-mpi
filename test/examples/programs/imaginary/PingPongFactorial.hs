{-# LANGUAGE ScopedTypeVariables #-}

{-
   Ping Pong Factorial.

   Two processes calculate the factorial of the input.

   Note: this is not a fast, nor sensible way to compute factorials.
   It is merely intended to be used to demonstrate point-to-point
   communications.
-}
module Main where

import Control.Monad (when)
import Control.Parallel.MPI.Serializable
import Control.Parallel.MPI.Common
import Control.Applicative ((<$>))
import Data.Char (isDigit)

type Msg = Either (Integer, Integer, Integer) Integer

zero, one :: Rank
zero = 0
one = 1

main :: IO ()
main = mpiWorld $ \size rank -> do
   when (size == 2) $ do
      when (rank == zero) $ do
         n <- getNumber
         send commWorld one unitTag (Left (n, 0, 1) :: Msg)
      result <- factorial $ switch rank
      when (rank == zero) $ print result

factorial :: Rank -> IO Integer
factorial rank = do
   (msg :: Msg) <- fst <$> recv commWorld rank unitTag
   case msg of
      Right answer -> return answer
      Left (n, count, acc)
         | count == n -> do
              send commWorld rank unitTag (Right acc :: Msg)
              return acc
         | otherwise -> do
              let nextCount = count + 1
              send commWorld rank unitTag (Left (n, nextCount, nextCount * acc) :: Msg)
              factorial rank

switch :: Rank -> Rank
switch rank
   | rank == zero = one
   | otherwise = zero

getNumber :: IO Integer
getNumber = do
    line <- getLine
    if all isDigit line
       then return $ read line
       else return 0
