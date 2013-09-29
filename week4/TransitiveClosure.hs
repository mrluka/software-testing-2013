module TransitiveClosure where

import SetOrd
import Data.List
import SetOperations
import Techniques

--
-- Task 4: (time spent: 8h)
-- Suppose we implement binary relations as list of pairs, Haskell type [(a,a)].
-- Assume the following defnitions:
-- 

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

--
-- Use this to implement a function trClos :: Ord a => Rel a -> Rel a 
-- that gives the transitive closure of a relation, where the relation is
-- represented as a list of pairs.
-- E.g., ttrClos [(1,2),(2,3),(3,4)] should give
-- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].
-- 

s1 = [(1,2),(2,3),(3,4),(4,5)]
s2 = [(3,1),(3,5),(5,9),(1,9)]
s3 = [(1,2),(2,3)]

trClos ::Ord a => Rel a -> Rel a
trClos rel = do trans1 <- return (nub (rel ++ (rel @@ rel)))
                if length trans1 == length rel
                   then rel
                   else trClos trans1
                               
--helper : transform set to relation
setToRel :: Ord a => Set (a,a) -> Rel a
setToRel x = setToRelR x []

setToRelR :: Ord a => Set(a,a) -> Rel a -> Rel a
setToRelR (Set []) y = y
setToRelR (Set (x:xs)) y = x : setToRel (Set xs)

-- helper : transform relation to set
relToSet :: Ord a => Rel a -> Set (a,a)
relToSet x  = relToSetR x emptySet

relToSetR :: Ord a => Rel a -> Set (a,a) -> Set (a,a)
relToSetR [] y = y
relToSetR (x:xs) y = relToSetR xs (insertSet x y)


-- > ---- Task 5:
-- time spent 6h (plus 2h Task 2)
-- Test the function trClos from the previous exercise. Devise your own 
-- test method for this. Try to use random test generation. Define reasonable properties to test

-----manual
manualTest = (not (isTrans s1) &&  isTrans (trClos s1))

-----automatic
simpleAutoTest :: IO Bool
simpleAutoTest = autoTest 100

-- maybe more complex tests with more parameters :test count, pair count [(),....()n], int element range n (x E n)
autoTest :: Int ->IO Bool
autoTest testCount  = if testCount > 0 
		       then do 
				randRel <- getRandomRelation 4  -- relation pair count
				testRes <-  autoTest (testCount -1)
				testResR <- autoTestR randRel
				return (testRes && testResR)
                      else return True

autoTestR :: Rel Int -> IO Bool
autoTestR x = return  ((length x == length (trClos x)) || (isTrans (trClos x)))

getRandomRelation ::  Int -> IO (Rel Int) 
getRandomRelation  0 = return []
getRandomRelation  n = do fstX <- getRandomInt 9 -- x range
			  fstY <- getRandomInt 9 -- y range				
			  randPairs <- getRandomRelation (n-1) 			
			  return ((fstX, fstY) : randPairs)

-- -- -- -- Transitive check method #2 (From book page 175. transR)
-- slightly transformed because different implementation of "Rel":
-- the original function was dependent on the order of elements in the relation
isTrans :: Ord a => Rel a -> Bool
isTrans x = isTrans2 (relToSet x)

isTrans2 :: Ord a => Set (a,a) -> Bool
isTrans2 (Set []) = True
isTrans2 (Set s) = and [trans pair (Set s) | pair <- s] where       
               trans (x,y) (Set r) = and [inSet (x,v) (Set r) | (u,v) <- r, u ==y]

--------------------------
-- --- -- test

testT :: IO Bool
testT = do randClo <- generateRandomTransClosures
           let isT = isTrans randClo
           return isT

generateRandomTransClosures :: IO (Rel Int)
generateRandomTransClosures = do randRel <- nGenRandRel 3 2
                                 let transClo = trClos randRel
                                 print transClo
                                 return transClo

nGenRandRel :: Int ->  Int -> IO (Rel Int) -- r = range; c = pairsCount
nGenRandRel r 0 = return []
nGenRandRel r c =  do randX <- getRandomInt r 
                      randY <- getRandomInt r
                      randRel <- nGenRandRel r (c-1)
                      let noDu=  nub ((randX,randY): randRel)
                      return noDu

xnGenRandRelations :: Int -> Int  -> IO (Rel Int)-- r = range; p = pairCount
xnGenRandRelations r p  = do randRel <- nGenRandRel r p
                             print ("" ++ show randRel)
                             return randRel
