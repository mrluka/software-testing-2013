module TestPermutationsCK where
import PermutationCK
import GenIntListLH
                        
-- ---- ---- ---- ---- ----
p1 = [0,0,2]
p2 = [0,2,0]
p3 = [2,0,0]

np1 = [2,2,0]
np2 = [0,2]
np3 = [2,2,2,3]


-- first: testing with manual written lists in manual assigned combinations
testPermutations :: Bool
testPermutations = test1 && test2 && test3

-- manually test positive cases with predefined arrays
test1 :: Bool
test1 = isPermutation p1 p2 &&
	isPermutation p1 p3 &&
	isPermutation p2 p3 &&
	isPermutation p2 p1 &&
	isPermutation p3 p1 &&
	isPermutation p3 p2

-- manually test negative cases with predefined arrays
test2 :: Bool
test2 = not (isPermutation p1 np1) &&
	not (isPermutation p1 np2) &&
	not (isPermutation p1 np3) &&
	not (isPermutation p2 np1) &&
	not (isPermutation p2 np2) &&
	not (isPermutation p2 np3) &&
	not (isPermutation p3 np1) &&
	not (isPermutation p3 np2) &&
	not (isPermutation p3 np3)

-- manually test properties of predefined arrays
test3 :: Bool
test3 = atests p1 p2 p3 && 
	atests p1 p2 np3 && 
	atests p1 np2 np3 && 
	atests p1 np2 p3 &&
	atests np1 p2 p3 && 
	atests np1 p2 np3 && 
	atests np1 np2 np3 && 
	atests np1 np2 p3 

-- run all property tests for the three given lists
atests :: Eq a => Ord a=> [a] -> [a] -> [a] -> Bool
atests z x y = 	atests1 x y z &&
		atests2 x y z &&
		atests3 x y z &&
		atests4 x y z &&
		atests5 x y z &&
		atests6 x y z

-- run 1st property test in every possible combination for the three given lists
atests1 :: Eq a => Ord a => [a] -> [a] -> [a] -> Bool
atests1 x y z = atest1 x y && 
		atest1 x z && 
		atest1 y z &&
		atest1 y x && 
		atest1 z x && 
		atest1 z y
-- run 2nd property test in every possible combination for the three given lists
atests2 :: Eq a => Ord a => [a] -> [a] -> [a] -> Bool
atests2 x y z = atest2 x y z && 
		atest2 x z y && 
		atest2 z x y &&
		atest2 z y x && 
		atest2 y z x && 
		atest2 y x z
-- run 3rd property test in every possible combination for the three given lists
atests3 :: Eq a => Ord a => [a] -> [a] -> [a] -> Bool
atests3 x y z = atest3 x y && 
		atest3 x z && 
		atest3 y z &&
		atest3 y x && 
		atest3 z x && 
		atest3 z y
		
atests4 :: Eq a => Ord a => [a] -> [a] -> [a] -> Bool
atests4 x y z = atest4 x y && 
		atest4 x z && 
		atest4 y z &&
		atest4 y x && 
		atest4 z x && 
		atest4 z y
		
atests5 :: Eq a => Ord a => [a] -> [a] -> [a] -> Bool
atests5 x y z = atest5 x y && 
		atest5 x z && 
		atest5 y z &&
		atest5 y x && 
		atest5 z x && 
		atest4 z y 

atests6 x y z =  isPermutation x x &&
		 isPermutation y y &&
		 isPermutation z z
-- Testable Properties

-- testable property #1 : lists must have same lengths
-- if isPermutation x z then countElements x == countElements y else True
atest1 :: Eq a => Ord a => [a] -> [a] -> Bool
atest1 p q = if isPermutation p q then countElements p == countElements q else True

-- testable property #2 : transitivity if a and b are permutations and a and c are permutations, then b and c are permutations
-- if isPermutation x y && isPermutation y z then isPermutation x z else True
atest2 :: Eq a => Ord a => [a] -> [a] -> [a] -> Bool
atest2 x y z = 	if isPermutation x y && isPermutation y z 
		then isPermutation x z 
		else True

-- testable property #3 : commutativity if a and b are permutations then b and a are also permutations
-- if isPermutation x y then isPermutation y x  else  True 
atest3 :: Eq a => Ord a => [a] -> [a] -> Bool
atest3 p q = 	if isPermutation p q 
		then isPermutation q p 
		else True

-- testable property #4 : if a and b are permutations then every element that exists in a must also exist in b and vice versa
atest4 :: Eq a => Ord a => [a] -> [a] -> Bool
atest4 x y = if isPermutation x y
		  then atest4a x y && atest4a y x
		  else True

-- Helper function to check that all elements in a exist in b
atest4a [] [] = True  
atest4a [] (y:ys) = True
atest4a (x:xs) [] = False
atest4a (x:xs) (y:ys) = existsIn x (y:ys) && atest4a xs (y:ys)

-- testable property #5 : if a == b then a must be a permutation of b
atest5 :: Eq a => Ord a => [a] -> [a] -> Bool
atest5 x y = if x == y then isPermutation x y else True

-- Helper function to check if an element exists in an array
existsIn :: Eq a => a -> [a] -> Bool
existsIn x [] = False
existsIn x (y:ys) = x == y || existsIn x ys

-- Helper function to count elements in an array
countElements :: Eq a => [a] -> Int
countElements [] = 0
countElements (x:xs) = 1 + countElements xs


 
-- second: testing with randomly generated lists
-- make 1.000.000.000 runs to find some permutations takes too long, 10000 is feasible
runPermTests = runPermTestsR 10000

runPermTestsR :: Int -> IO Bool
runPermTestsR x = if x > 0 then runPermTestsRHelper (x -1)  else return True

runPermTestsRHelper :: Int -> IO Bool
runPermTestsRHelper n = do res1 <- testRandomPermutations
                           res2 <- runPermTestsR n 
                           return (res1 && res2)

testRandomPermutations :: IO Bool
testRandomPermutations = do list1 <-  genIntList; 
                            list2 <-  genIntList;
                            list3 <-  genIntList; 
                            return (atests list1 list2 list3) --list1 list2 list3  let testResult =