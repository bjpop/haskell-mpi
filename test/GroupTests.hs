module GroupTests (groupTests) where

import TestHelpers

groupTests :: Rank -> [(String,TestRunnerTest)]
groupTests rank =
  [ groupTestCase rank "groupRank" groupRankTest
  , groupTestCase rank "groupSize" groupSizeTest
  , groupTestCase rank "groupUnionSelf" groupUnionSelfTest
  , groupTestCase rank "groupIntersectionSelf" groupIntersectionSelfTest
  ]

-- mpiTestCase :: Rank -> String -> (Rank -> IO ()) -> (String,TestRunnerTest)

groupTestCase :: Rank -> String -> (Rank -> Group -> IO ()) -> (String,TestRunnerTest)
groupTestCase rank str test =
   mpiTestCase rank str $ \rank -> do
      group <- commGroup commWorld
      test rank group

-- Test if the rank from commWorld is the same as the rank from a group created
-- from commWorld.
groupRankTest :: Rank -> Group -> IO ()
groupRankTest rank group = do
   gRank <- groupRank group
   gRank == rank @? ("Rank == " ++ show rank ++ ", but group rank == " ++ show gRank)

-- Test if the size of commWorld is the same as the size of a group created
-- from commWorld.
groupSizeTest :: Rank -> Group -> IO ()
groupSizeTest _rank group = do
   cSize <- commSize commWorld
   gSize <- groupSize group
   gSize > 0 @? ("Group size " ++ show gSize ++ " not greater than zero")
   gSize == cSize @? ("CommWorld size == " ++ show cSize ++ ", but group size == " ++ show gSize)

-- Test if the union of a group with itself is the identity on groups
-- XXX is it enough to just check sizes?

groupUnionSelfTest :: Rank -> Group -> IO ()
groupUnionSelfTest _rank group = groupIdentitySelfTest group groupUnion "union"

groupIntersectionSelfTest :: Rank -> Group -> IO ()
groupIntersectionSelfTest _rank group = groupIdentitySelfTest group groupIntersection "intersection"

groupIdentitySelfTest :: Group -> (Group -> Group -> IO Group) -> String -> IO ()
groupIdentitySelfTest group groupOp opString = do
   gSize <- groupSize group
   uGroup <- groupOp group group
   uSize <- groupSize uGroup
   gSize == uSize @? ("Group size " ++ show gSize ++ ", " ++ opString ++ "(Group,Group) size == " ++ show uSize)

