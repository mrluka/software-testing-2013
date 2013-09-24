module SetOperationsCK where

import SetOrd

-- 
-- Task 3 : Implement operations for set intersection, set union and set difference
-- Next, use automated random testing to check that your implementation is correct.
--

-- @todo implementation
intersection :: Ord a => Set a -> Set a -> Set a
intersection x y = y

-- @todo implementation
union :: Ord a => Set a -> Set a -> Set a
union x y = y

-- @todo implementation
difference :: Ord a => Set a -> Set a -> Set a
difference x y = y

testOperations :: Bool
testOperations = testIntersection && testUnion && testDifference

-- @todo implementation
testIntersection :: Bool
testIntersection = False

-- @todo implementation
testUnion :: Bool
testUnion = False

-- @todo implementation
testDifference :: Bool
testDifference = False