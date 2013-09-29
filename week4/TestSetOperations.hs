module TestSetOperations where
import SetOrd
import RandomSets
import SetOperations

-- wrapper for manual test functions

manualTests :: Bool
manualTests = testIntersectionM && testUnionM && testDifferenceM

-- preparation of manual testing

t1 = insertSet 10 (insertSet 8 (insertSet 6 emptySet))
t2 = insertSet 11 (insertSet 9 (insertSet 6 emptySet))
t3 = insertSet 11 (insertSet 9 (insertSet 7 emptySet))

-- manual testing of intersection

testIntersectionM :: Bool
testIntersectionM = isEmpty (intersection t1 t3)
  && not (isEmpty (intersection t1 t2))
  && inSet 6 (intersection t1 t2)
  && not (inSet 6 (intersection t1 t3))
  && not (inSet 11 (intersection t1 t3))
  
-- manual testing of union
testUnionM :: Bool
testUnionM = not (isEmpty (t1 `union` t2)) && 
	    inSet 10 (t1 `union` t2) && 
	    inSet 6 (t1 `union` t2) &&
	    not (inSet 12 (t1 `union` t2))
	    
-- manual testing of difference

testDifferenceM :: Bool
testDifferenceM = isEmpty (difference t1 t1) &&
		  not (isEmpty (difference t1 t2)) && 
		  inSet 10 (difference t1 t2) &&
		  not (inSet 6 (difference t1 t2))
	    
 
-- wrapper to run 100.000 automatic tests

autoTests :: IO Bool
autoTests = autoTestsR 100000

-- run automatic tests recursive x times

autoTestsR :: Int -> IO Bool
autoTestsR x = if x > 0
		then 
		doAutoTests x --(autoTestsR2) && (autoTestsR (x - 1)))	  
		else 
		getIOTrue
		
-- runner for the automatic tests (will call back to autoTestsR)

doAutoTests :: Int -> IO Bool
doAutoTests x = do 
		   y <- autoTestsR (x -1)
		   z <- testUnionA
		   w <- testIntersectionA
		   v <- testDifferenceA
		   return (z && y && w && v)
		   
-- automated testing of intersection

testIntersectionA :: IO Bool
testIntersectionA = do  x <- randomS
		        y <- randomS
			return (testIntersectionA1 x y)
			
testIntersectionA1 :: Ord a => Set a -> Set a -> Bool
testIntersectionA1 x y = testIntersectionA2 x y (intersection x y)

testIntersectionA2 :: Ord a => Set a -> Set a -> Set a -> Bool
testIntersectionA2 (Set []) y z = True
testIntersectionA2 (Set (x:xs)) y z = inSet x y == inSet x z 
						  && testIntersectionA2 (Set xs) y z
						  
-- automated testing of union

testUnionA :: IO Bool
testUnionA = do x <- randomS
		y <- randomS
		return (testUnionA1 x y)
		
testUnionA1 :: Ord a => Set a-> Set a->  Bool
testUnionA1 x y = testUnionA2 x y (x `union` y)

testUnionA2 :: Ord a => Set a -> Set a -> Set a -> Bool
testUnionA2 (Set []) (Set []) z = True
testUnionA2 (Set []) y z = True
testUnionA2 x (Set []) z = True
testUnionA2 (Set (x:xs)) (Set (y:ys)) z = inSet x z
					  && inSet y z
					  && testUnionA2 (Set xs) (Set ys) z

-- automated testing of difference

testDifferenceA :: IO Bool
testDifferenceA =  do x <- randomS
		      y <- randomS
		      return (testDifferenceA1 x y)
		
testDifferenceA1 :: Ord a => Set a -> Set a ->  Bool
testDifferenceA1 x y = testDifferenceA2 x y (difference x y)

testDifferenceA2 :: Ord a => Set a -> Set a -> Set a -> Bool
testDifferenceA2 x y (Set []) = True
testDifferenceA2 x y (Set (z:zs))  = (inSet z x /= inSet z y) && testDifferenceA2 x y (Set zs)


--
-- Helpers
--
		
-- dirty helper to have access to "True" in IO 

getIOTrue :: IO Bool
getIOTrue = do z <- randomS
	       return (getTrue z)
	       
-- dirty helper to have access to "True" in IO 

getTrue x = True


