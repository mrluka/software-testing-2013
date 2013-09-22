module Sol3 
where
import Data.List
import Techniques

-- > --------- Ex 3
-- Consult the course slides of this week to write a generator for random integer lists. 
-- The type should be: genIntList :: IO [Int]

genIntList :: IO [Int]
genIntList = do d <- getRandomInt 20
                genIntListHelper d 10 -- random <boundaries>, fixed (10) length 
--                genIntListHelper d d -- list <length> same as random <boundaries>
--                genIntListHelper 10 d -- random <length>, fixed <boundaries> (10) 

-- #1: d- random Int boundaries (domain?)
-- #2: n- counter (list length) 
genIntListHelper :: Int -> Int -> IO [Int]
genIntListHelper _ 0 = return []
genIntListHelper d n = do 
                     f <- getRandomInt d
                     fs <- genIntListHelper d (n-1) 
                     return (f:fs)

-- > --------- Ex 4
-- Write a function that returns True if its arguments are permutations of each other.  

isPermutation :: Eq a => [a] -> [a] -> Bool          
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation x y = isPermu x y

isPermu :: Eq a => [a] -> [a] -> Bool
isPermu [] [] = True -- If both lists are empty, it is a Permutation
isPermu (x:xs)(y:ys) | length (x:xs) /= length (y:ys) = False -- Compare length of both lists, must be the same for permutation
                     | count x (x:xs) == count x (y:ys) = isPermu (rmItemFromList x (x:xs)) (rmItemFromList x (y:ys)) -- If occurences of current element are equal in both lists, remove the equal element from both lists and call function again but with altered list which does not contain previously handled elements. If it is a permutation, both lists shrink simultaneously, which means that the size of both lists is always equal.
                     | otherwise = False -- All other cases mean that it is not a permutation

-- HELPER FUNCTIONS 
rmItemFromList :: Eq a => a -> [a] -> [a]
rmItemFromList  _ []                = []
rmItemFromList x (y:ys) | x == y    = rmItemFromList x ys
                     | otherwise = y : rmItemFromList x ys

count :: Eq a => a -> [a] -> Int
count y [] = 0
count  y (x:xs) | y == x = 1 + count y xs
 		  | otherwise = count y xs


-- > Ex 5         
-- Define some testable properties for the isPermutation function. 
-- Use random generator for integer lists to test isPermutation
-- --
-- For the test cases

permutationTest :: IO()
permutationTest = runTestsuite 100

-- call all test scenarios from here 
runTestsuite :: Int -> IO()
runTestsuite n = do a <- genIntList
               b <- genIntList
               runner a b

-- !! Possible: Function that decides weather the test should yield true || false.
-- Combined with following test scenarios?!
-- Tests:
-- 1) Generate 1 random Int List and use as Argument for both lists, every relation should be a permutation
-- 2) Generate 1 random Int List as parameter a, and only use empty lists for parameter b (& vice-versa)
-- 3) Generage 2 random Int Lists with both, a random Int range but also a random List length
-- 4) Generate 2 random Int Lists with a fixed Int range but random list length (and vice-versa)
-- 5) Generate 2 random Int lists with both fixed ranges. 

-- > ! !  ! define (as in Techniques.hs) lambda calculus to describe which test function to use?!
--          And then for each Scenario one lambda ...?
test1 :: 