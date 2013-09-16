--UVA Software Testing 2013
--Group Di3 (Christian König, Luka Hale)
--Chapter 1 implementing exercises from "The Haskell Road" (by Jan van Eijck, Kees Doets)

module Sol1 where
import GS

-- > Ex 1.9 (Function that gives the maximum of [Int])
maxInt :: [Int] -> Int
maxInt [] = error "Error! [Int] argument expected" --Maybe we could also return -1. 
maxInt [x] = x -- if only one item in list (singleton list)
maxInt (x:xs) = max x (maxInt xs) --Uses predefined max(first in list, maxInt(rest of list)). Contains recursive call

-- > Ex 1.10 (Removes first occurence of Int m from [Int]
removeFst :: Int -> [Int] -> [Int] -- type declaration
removeFst _ []  = [] -- if list is empty, _ as wildcard
removeFst m (x:xs)  | x == m =  xs --if first item in list == item to search for, then return xs (rest of list without head)
                    | otherwise = x : removeFst m xs
 --if none of previous holds, recursively call function again with m xs. (No empty list and the search was also false on the first item of list. Now, call removeFst1 again with the rest of the initial list, starting without head of initial list.



-- > Ex 1.13 (Count number of occurences of Char in String)
count :: Char -> String -> Int
count y [] = 0
count  y (x:xs) | y == x = 1 + count y xs
 		  | otherwise = count y xs


-- > Ex 1.14 (Transform String into String (Blowup): "ABC" -> "ABBCCC")
blowup :: String -> String
blowup xs = printBlowup xs 1 

printBlowup :: String -> Int -> String
printBlowup [] _ = []
printBlowup (x:xs) c = take c (repeat x) ++ printBlowup xs (c+1)

-- > Ex 1.15 (Sort [String] in alphabetic order)
srtString ::  [String] -> [String]
srtString [] = []
srtString (x:xs) = minimum (x:xs) : srtString (rmStrFrmLst (minimum(x:xs)) (x:xs))
-- Take minimum (predefined) String from list of Strings and put this by using ":" to tail of list (Initially empty). Then recursively cann srtString again but without the previous found minimum occurrence, by calling helper function rmStrFrLst 
-- Helper function to remove a String from a List of Strings
rmStrFrmLst :: String -> [String] -> [String]
rmStrFrmLst  _ []                = []
rmStrFrmLst x (y:ys) | x == y    = rmStrFrmLst x ys
                     | otherwise = y : rmStrFrmLst x ys


-- > Ex 1.17 (Check whether a String is a substring of another String )
substring :: String -> String -> Bool
substring _ [] = False
substring [] _ = False -- Maybe this should return true? Is an empty String a substring of another?
substring (x:xs) (y:ys) | prefix (x:xs) (y:ys) = True
                        | otherwise = substring (x:xs) ys


-- > Ex 1.20 (Function that takes a list of lists and returns list of corresponding list length)
lengths:: [[a]]->[Int]
lengths [] = [] -- Or maybe error is better?
lengths (x:xs) = map length (x:xs)

-- > Ex 1.21 (Function that takes list of lists and returns the sum of their length)
sumLengths :: [[a]] -> Int
sumLengths [] = 0
sumLengths (x:xs) = sum(lengths (x:xs))


-- > Ex 1.1 (Try out calculations)
-- 2*2 -> 4
-- 1+2+3+4 -> 10
--(1+2)*2 -> 6
--(1+2)*2 -2 -> 4
--(1+1)*(2*3) -> 12

-- > Ex 1.3 (Put divides in file prime.hs, but already in GS.hs)
-- > Ex 1.4 ()
-- Exercise 1.4
-- Replace k^2 > n with k^2>= n, would it make any difference?
-- WRONG: It would make a difference because then the function would work for all n.
-- If called "ld 4", then, in the first version there would be a wrong result because ldf would
-- recursively call itself again with ldf 3 4 what will not give a valid result, so the secont solution with 
-- k^2 >= n is right.
-- RIGHT: No difference, first line captures the case with "2"
-- > Ex 1.4 part 2 (in GS.hs)
-- > Ex 1.5 (Add definitions to prime.hs, ...but are already in GS.hs)
-- > Ex 1.6
-- rem :: Integer -> Integer -> Bool
-- > Ex 1.7
-- The type definition of divides is: Integer-> Integer-> Bool.
-- "divides 5" is type of Integer-> Integer because that is what the function returns with one parameter. By adding another parameter, such as "divides 5 7", then first "divides 5" with argument "5" will be executed and produces the result Integer-> Bool, which is then called with "7" as formal argument, which then produces Bool.

-- > Ex 1.18
--1 [String]
--a) :t ["a","a"]
--b) :t (["luka"]++["la"])

--2(Bool,String)
--a) :t (True,"la")
--b) :t ((1<2),"l")

--3 [(Bool,String)]
--a) :t [(True,"la"),(False,"aaaa")]
--b) :t [(True,"la"), ((1<2),"l")]

--4 ([Bool],String)
--a) :t (([True,False,True],"luka"))
--b) :t (([True,1>2,4 /= 3],"luka"))

--5 Bool->Bool
--a) :t not

-- > Ex 1.19
--head :: [a] -> a
-- head "abc" -> 'a' (only the first)

--last :: [a] -> a
-- last "abc" -> 'c' (only the last)

--init :: [a] -> [a]
-- init "2345" -> "234" (without the last)

--fst :: (a, b) -> a
-- fst ("fg","a") -> "fg" (first of pair / tupel)

--(++) :: [a] -> [a] -> [a]
-- (++) "a" "b" (concatination)

--flip :: (a -> b -> c) -> b -> a -> c
--flip (/) 4 2 -> 0.5 (would expect 2 but because of the argument flipping, it returns 0.5 insteand of 2)

--flip (++) :: [a] -> [a] -> [a]
--flip (++) "a" "b" -> "ba" (Usually (++) "a" "b" would return "ab" but because of flip, it's "ba"

-- > Ex 1.24
-- By removing "n" fron ldp', nothing changes compared to the initial version (with the n). Probably because of the type definition the argument (in first version represented by n) is parsed automatically
--ldp' :: Integer -> Integer
--ldp'  = ldpf' primes1 

--ldpf' :: [Integer] -> Integer -> Integer
--ldpf' (p:ps) n | rem n p == 0 = p
--          | p^2 > n      = n
--          | otherwise = ldpf' ps n
