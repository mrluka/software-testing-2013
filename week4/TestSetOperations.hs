module TestSetOperations where
import SetOrd
import RandomSetsCK
import SetOperationsCK

-- wrapper for manual test functions
manualTests :: Bool
manualTests = testIntersection && testUnion && testDifference

-- preparation of manual testing
t1 = insertSet 10 (insertSet 8 (insertSet 6 emptySet))
t2 = insertSet 11 (insertSet 9 (insertSet 6 emptySet))
t3 = insertSet 11 (insertSet 9 (insertSet 7 emptySet))

-- manual testing of intersection
testIntersection :: Bool
testIntersection = (isEmpty (intersection t1 t3) == True) 
  && (isEmpty (intersection t1 t2) == False) 
  && (inSet 6 (intersection t1 t2) == True) 
  && (inSet 6 (intersection t1 t3) == False) 
  && (inSet 11 (intersection t1 t3) == False)
  
-- manual testing of union
testUnion :: Bool
testUnion = ((isEmpty (union t1 t2)) == False) && 
	    ((inSet 10 (union t1 t2)) == True) && 
	    ((inSet 6 (union t1 t2)) == True) &&
	    ((inSet 12 (union t1 t2)) == False)
	    
-- manual testing of difference
testDifference :: Bool
testDifference = ((isEmpty (difference t1 t1)) == True) &&
		  ((isEmpty (difference t1 t2)) == False) && 
		  ((inSet 10 (difference t1 t2)) == True) &&
		  ((inSet 6 (difference t1 t2)) == False)
	    
 
-- wrapper to run 100.000 automatic tests
autoTests :: IO Bool
autoTests = autoTestsR 100000

-- run automatic tests recursive
autoTestsR :: Int -> IO Bool
autoTestsR x = if(x > 0) 
		then 
		doAutoTests x --(autoTestsR2) && (autoTestsR (x - 1)))	  
		else 
		getIOTrue
		
-- runner for the automatic tests
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
			
testIntersectionA1 :: Ord a => Set (a) -> Set (a) -> Bool
testIntersectionA1 x y = testIntersectionA2 x y (intersection x y)

testIntersectionA2 (Set []) (Set []) z = True
testIntersectionA2 (Set []) y z = True
testIntersectionA2 (Set (x:xs)) y z = (existsIn x y == (existsIn x z) )
						  && (testIntersectionA2 (Set xs) y z)
						  
-- automated testing of union
testUnionA :: IO Bool
testUnionA = do x <- randomS
		y <- randomS
		return (testUnionA1 x y)
		
testUnionA1 :: Ord a => Set(a) -> Set(a) ->  Bool
testUnionA1 x y = testUnionA2 x y (union x y)

testUnionA2 (Set []) (Set []) z = True
testUnionA2 (Set []) y z = True
testUnionA2 x (Set []) z = True
testUnionA2 (Set (x:xs)) (Set (y:ys)) z = (existsIn x z) 
					  && (existsIn y z) 
					  && testUnionA2 (Set xs) (Set ys) z

-- automated testing of difference
testDifferenceA :: IO Bool
testDifferenceA =  do x <- randomS
		      y <- randomS
		      return (testDifferenceA1 x y)
		
testDifferenceA1 :: Ord a => Set(a) -> Set(a) ->  Bool
testDifferenceA1 x y = testDifferenceA2 x y (difference x y)

testDifferenceA2 x y (Set []) = True
testDifferenceA2 x y (Set (z:zs))  = ((existsIn z x) /= (existsIn z y)) && testDifferenceA2 x y (Set zs)


--
-- Helpers
--
existsIn :: Eq a => a-> Set(a) -> Bool
existsIn needle (Set []) = False
existsIn needle (Set (x:xs)) = needle == x || existsIn needle (Set xs)
		
-- dirty helper to have access to "True" in IO 	  
getIOTrue :: IO Bool
getIOTrue = do z <- testUnionA
	       return (getTrue z)
	       
-- dirty helper to have access to "True" in IO 	       
getTrue x = True


