module Main where

import Control.Parallel.MPI.Common
import Control.Parallel.MPI.Storable
import Data.Array.Storable
import Control.Monad (when, forM, forM_)
import Text.Printf
import System.Random
import Control.Applicative
import Foreign (sizeOf)

-- Fit a and b to the model y=ax+b. Return a, b, variance
linfit :: StorableArray Int Double -> StorableArray Int Double -> IO (Double, Double, Double)
linfit x y = do
  x_bnds <- getBounds x
  y_bnds <- getBounds y
  when (x_bnds /= y_bnds) $ error "x and y must have same length and dimensions"

  xs <- getElems x
  ys <- getElems y

  let n  = rangeSize x_bnds
      sx = sum xs
      sy = sum ys

      sxon = sx/(fromIntegral n)

      ts = map (\x -> x-sxon) xs
      sson = sum $ map (^2) ts

      a = (sum $ zipWith (*) ts ys)/sson
      b = (sy-sx*a)

      norm = sum $ map (^2) xs
      res  = zipWith (\x y -> y - a*x - b) xs ys
      varest = (sum $ map (^2) res)/(norm * (fromIntegral $ n-2))

  return (a,b,varest)

---- Main program
maxI = 10         -- Number of blocks
maxM = 500000     -- Largest block
block = maxM `div` maxI -- Block size

repeats = 10

main = mpi $ do
  numProcs <- commSize commWorld
  myRank   <- commRank commWorld

  when (myRank == zeroRank) $ do
    putStrLn $ printf "MAXM = %d, number of processors = %d" maxM numProcs
    putStrLn $ printf "Measurements are repeated %d times for reliability" repeats

  if numProcs < 2
    then putStrLn "Program needs at least two processors - aborting"
    else measure numProcs myRank

measure numProcs myRank = do
  putStrLn $ printf "I am process %d" ((fromRank myRank) :: Int)

  -- Initialize data
  a <- sequence $ replicate maxM $ getStdRandom(randomR(0,2147483647::Double))
  when (myRank == zeroRank) $ do putStrLn $ printf "Generating randoms: %d done" (length a)
  let elsize = sizeOf (undefined::Double)

  noelem  <- newArray (1, maxI) (0::Double) :: IO (StorableArray Int Double)
  bytes   <- newArray (1, maxI) (0::Double) :: IO (StorableArray Int Double)
  mintime <- newArray (1, maxI) (100000::Double) :: IO (StorableArray Int Double)
  maxtime <- newArray (1, maxI) (-100000::Double) :: IO (StorableArray Int Double)
  avgtime <- newArray (1, maxI) (0::Double) :: IO (StorableArray Int Double)

  cpuOH <- if myRank == zeroRank then do
    ohs <- sequence $ replicate repeats $ do
      t1 <- wtime
      t2 <- wtime
      return (t2-t1)
    let oh = minimum ohs
    putStrLn $ printf "Timing overhead is %f seconds." oh
    return oh
    else return undefined

  forM_ [1..repeats] $ \k -> do
    when (myRank == zeroRank) $ putStrLn $ printf "Run %d of %d" k repeats

    forM_ [1..maxI] $ \i -> do
      let m = block*(i-1)+1 :: Int
      writeArray noelem i (fromIntegral m)

      barrier commWorld
      msg <- newListArray (1,m) $ take m a :: IO (StorableArray Int Double)
      c <- newArray (1,m) 0 :: IO (StorableArray Int Double)

      barrier commWorld -- Synchronize all before timing
      if myRank == zeroRank then do
        t1 <- wtime
        send commWorld (toRank (1::Int)) unitTag msg
        recv commWorld (toRank (numProcs-1 :: Int)) unitTag c
        t2 <- wtime
        let diff = t2 - t1 - cpuOH
        curr_avg <- readArray avgtime i
        writeArray avgtime i $ curr_avg + diff/(fromIntegral numProcs)

        curr_min <- readArray mintime i
        curr_max <- readArray maxtime i
        when (diff < curr_min) $ writeArray mintime i diff
        when (diff > curr_max) $ writeArray maxtime i diff
        else do -- non-root processes. Get msg and pass it on
        recv commWorld (toRank $ (fromRank myRank)-1) unitTag c
        send commWorld (toRank $ ((fromRank myRank)+1) `mod` numProcs) unitTag c

  when (myRank == zeroRank) $ do
    putStrLn "Bytes transferred   time (micro seconds)"
    putStrLn "                    min        avg        max "
    putStrLn "----------------------------------------------"

    forM_ [1..maxI] $ \i -> do

      avgtime_ <- (round.(*10e6).(/(fromIntegral repeats))) <$> readArray avgtime i :: IO Int -- Average micro seconds
      mintime_dbl <- (*10e6) <$> readArray mintime i :: IO Double -- Min micro seconds
      maxtime_ <- round.(*10e6) <$> readArray maxtime i :: IO Int -- Max micro seconds
      let mintime_ = round mintime_dbl :: Int

      m <- readArray noelem i
      writeArray bytes   i ((fromIntegral elsize) * m)
      writeArray mintime i mintime_dbl

      putStrLn $ printf "%10d    %10d %10d %10d" ((round $ (fromIntegral elsize) * m)::Int) mintime_ avgtime_ maxtime_


    (tbw, tlat, varest) <- linfit bytes mintime
    putStrLn $ "\nLinear regression on best timings (t = t_l + t_b * bytes):"
    putStrLn $ printf "  t_b = %f\n  t_l = %f" tbw tlat
    putStrLn $ printf "  Estimated relative variance = %.9f\n" varest

    putStrLn $ printf "Estimated bandwith (1/t_b):  %.3f Mb/s" (1.0/tbw)
    mt0 <- readArray mintime 1
    b0 <- readArray bytes 1
    putStrLn $ printf "Estimated latency:           %d micro s" (round (mt0-b0*tbw) :: Int)
