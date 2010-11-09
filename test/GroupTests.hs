module GroupTests (groupTests) where

import TestHelpers
import Control.Parallel.MPI.Base

groupTests :: Rank -> [(String,TestRunnerTest)]
groupTests rank =
  [ groupTestCase rank "groupRank" groupRankTest
  , groupTestCase rank "groupSize" groupSizeTest
  , groupTestCase rank "groupUnionSelf" groupUnionSelfTest
  , groupTestCase rank "groupIntersectionSelf" groupIntersectionSelfTest
  , groupTestCase rank "groupDifferenceSelf" groupDifferenceSelfTest
  , groupTestCase rank "groupCompareSelf" groupCompareSelfTest
  , groupTestCase rank "groupCompareEmpty" groupCompareSelfEmptyTest
  , mpiTestCase rank "groupEmptySize" groupEmptySizeTest
  ]

groupTestCase :: Rank -> String -> (Rank -> Group -> IO ()) -> (String,TestRunnerTest)
groupTestCase rank str test =
   mpiTestCase rank str $ \rank -> do
      group <- commGroup commWorld
      test rank group

-- Test if the rank from commWorld is the same as the rank from a group created
-- from commWorld.
groupRankTest :: Rank -> Group -> IO ()
groupRankTest rank group = do
   let gRank = groupRank group
   gRank == rank @? "Rank == " ++ show rank ++ ", but group rank == " ++ show gRank

-- Test if the size of commWorld is the same as the size of a group created
-- from commWorld.
groupSizeTest :: Rank -> Group -> IO ()
groupSizeTest _rank group = do
   cSize <- commSize commWorld
   let gSize = groupSize group
   gSize > 0 @? "Group size " ++ show gSize ++ " not greater than zero"
   gSize == cSize @? "CommWorld size == " ++ show cSize ++ ", but group size == " ++ show gSize

-- Test if the union of a group with itself is the identity on groups
-- XXX is it enough to just check sizes?

groupUnionSelfTest :: Rank -> Group -> IO ()
groupUnionSelfTest _rank group =
   groupOpSelfTest group groupUnion "union" (==)

groupIntersectionSelfTest :: Rank -> Group -> IO ()
groupIntersectionSelfTest _rank group =
   groupOpSelfTest group groupIntersection "intersection" (==)

groupDifferenceSelfTest :: Rank -> Group -> IO ()
groupDifferenceSelfTest _rank group =
   groupOpSelfTest group groupDifference "difference" (\ _gSize uSize -> uSize == 0)

groupOpSelfTest :: Group -> (Group -> Group -> Group) -> String -> (Int -> Int -> Bool) -> IO ()
groupOpSelfTest group groupOp opString compare = do
   let gSize = groupSize group
       uGroup = groupOp group group
       uSize  = groupSize uGroup
   gSize `compare` uSize @? "Group size " ++ show gSize ++ ", " ++ opString ++ "(Group,Group) size == " ++ show uSize

groupCompareSelfTest :: Rank -> Group -> IO ()
groupCompareSelfTest _rank group = do
   let res = groupCompare group group
   res == Identical @? "Group compare with self gives non ident result: " ++ show res

groupCompareSelfEmptyTest :: Rank -> Group -> IO ()
groupCompareSelfEmptyTest _rank group = do
   let res = groupCompare group groupEmpty
   res == Unequal @? "Group compare with empty group gives non unequal result: " ++ show res

groupEmptySizeTest :: Rank -> IO ()
groupEmptySizeTest _rank = do
   let size = groupSize groupEmpty
   size == 0 @? "Empty group has non-zero size: " ++ show size
