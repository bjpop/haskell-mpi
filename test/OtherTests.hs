module OtherTests (otherTests) where

import TestHelpers

import Foreign.Storable (peek, poke)
import Foreign.Marshal (alloca)

otherTests :: Rank -> [(String,TestRunnerTest)]
otherTests _ = [ testCase "Peeking/poking Status" statusPeekPoke ]

statusPeekPoke :: IO ()
statusPeekPoke = do
  alloca $ \statusPtr -> do
    let s0 = Status minBound 2 3 maxBound 5
    poke statusPtr s0
    s1 <- peek statusPtr
    s0 == s1 @? ("Poked " ++ show s0 ++ ", but peeked " ++ show s1)
