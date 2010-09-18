{-# LANGUAGE ScopedTypeVariables #-}

{-
   Ping Pong Factorial.

   Two processes calculate the factorial of the input.
-}
module Main where

import Control.Monad (when)
import Control.Parallel.MPI.Serializable
import Control.Parallel.MPI.Common
import Control.Applicative ((<$>))
import Data.Char (isDigit)

type Msg = Either (Integer, Integer, Integer) Integer

zero, one :: Rank
zero = toRank 0
one = toRank 1

main :: IO ()
main = mpiWorld $ \size rank -> do
   when (size == 2) $ do
      when (rank == zero) $ do
         n <- getNumber
         send commWorld one unitTag (Left (n, 0, 1) :: Msg)
      result <- factorial rank
      when (rank == zero) $ print result

factorial :: Rank -> IO Integer
factorial rank = do
   let other = switch rank
   (msg :: Msg) <- fst <$> recv commWorld other unitTag
   case msg of
      Right answer -> return answer
      Left (n, count, acc)
         | count == n -> do
              send commWorld other unitTag (Right acc :: Msg)
              return acc
         | otherwise -> do
              let nextCount = count + 1
              send commWorld other unitTag (Left (n, nextCount, nextCount * acc) :: Msg)
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
