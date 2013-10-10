module PermutationCK where

--
-- how to find out if permutation? Let's try sorting and comparing arrays
--

isPermutation :: Eq a => Ord a => [a] -> [a] -> Bool
isPermutation x [] = False
isPermutation [] y = False
isPermutation x y = arrayEquals (srt x) (srt y)
-- VVZ: very old school. How about this?
-- VVZ: isPermutation x y = (sort x) == (sort y)

arrayEquals :: Eq a => Ord a=> [a] -> [a] -> Bool
arrayEquals [] [] = True
arrayEquals (x:xs) [] = False
arrayEquals [] (y:ys) = False
arrayEquals (x:xs) (y:ys) = (x == y) && arrayEquals xs ys

srt :: Eq a => Ord a=> [a] -> [a]
srt [] = []
srt xs = m : srt (removeFst m xs) where m = minimum xs
      
removeFst :: Eq a => Ord a=> a -> [a] -> [a]
removeFst y [] = []
removeFst y (x:xs) = if y == x then xs else x : removeFst y xs  