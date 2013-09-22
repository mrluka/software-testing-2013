module TestPermutationsLH where

-- > Ex 5         
-- Define some testable properties for the isPermutation function. 
-- Use random generator for integer lists to test isPermutation
-- --
-- For the test cases

--permutationTest :: IO()
--permutationTest = runTestsuite 100

-- call all test scenarios from here 
--runTestsuite :: Int -> IO()
--runTestsuite n = do a <- genIntList
--               b <- genIntList
--               runner a b

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
--test1 :: 
