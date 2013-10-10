module PermutationLH where

-- > --------- Ex 4
-- Write a function that returns True if its arguments are permutations of each other.  

isPermutation :: Eq a => [a] -> [a] -> Bool          
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation x y = isPermu x y
-- VVZ: what is the difference between these two functions? They seem to play the same role, take the same arguments, and you even cover the same base case
isPermu :: Eq a => [a] -> [a] -> Bool
isPermu [] [] = True -- If both lists are empty, it is a Permutation
isPermu (x:xs)(y:ys) | length (x:xs) /= length (y:ys) = False -- Compare length of both lists, must be the same for permutation
                     | count x (x:xs) == count x (y:ys) = isPermu (rmItemFromList x (x:xs)) (rmItemFromList x (y:ys)) -- If occurences of current element are equal in both lists, remove the equal element from both lists and call function again but with altered list which does not contain previously handled elements. If it is a permutation, both lists shrink simultaneously, which means that the size of both lists is always equal.
                     | otherwise = False -- All other cases mean that it is not a permutation

-- HELPER FUNCTIONS 
-- VVZ: what is the difference between this and removeFst from the first week?
rmItemFromList :: Eq a => a -> [a] -> [a]
rmItemFromList  _ []                = []
rmItemFromList x (y:ys) | x == y    = rmItemFromList x ys
                     | otherwise = y : rmItemFromList x ys

count :: Eq a => a -> [a] -> Int
count y [] = 0
count  y (x:xs) | y == x = 1 + count y xs
 		  | otherwise = count y xs

-- VVZ: again, very rigid imperative thinking! Don't think algorithms, don't think steps, think mapping, think data flow!
count' :: Eq a => a -> [a] -> Int
count' y = length . filter (== y)
