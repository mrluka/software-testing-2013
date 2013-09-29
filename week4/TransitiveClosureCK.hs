module TransitiveClosureCK where

import SetOrd
import Data.List
import SetOperationsCK
import Techniques
--
-- Task 4: (time spent: 6h)
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

-- @todo cleanup implementation
trClos :: Ord a => Rel a -> Rel a
trClos x = setToRel ( trClos1 ( relToSet x ) )

trClos1 :: Ord a => Set (a,a) -> Set (a,a)
trClos1 x = x `SetOperationsCK.union` trClos2 x emptySet

trClos2 :: Ord a => Set(a,a) -> Set(a,a) -> Set(a,a)
trClos2 (Set []) y = y
trClos2 (Set (x:xs)) y = trClos2 (Set xs) (trClos3 (Set (x:xs)) x y )

trClos3 (Set []) current z = z
trClos3 (Set (x:xs)) current z = trClos4 current (getAllSuccessors (Set (x:xs)) (getFirst x)) z

trClos4 current (Set []) z = z
trClos4 current (Set (y:ys)) z = trClos4 current (Set ys) (trClos5 current y z)

trClos5 current y z =  if inSet (getFirst current,y) z
				      then z 
				      else insertSet (getFirst current,y) z

suc = getSuccessors (relToSet s1) 1
-- returns the direct successors
getSuccessors :: Ord a => Set (a,a) -> a -> Set a
getSuccessors x lookup = getSuccessorsR x lookup emptySet
  
getSuccessorsR :: Ord a => Set (a,a) -> a -> Set a -> Set a
getSuccessorsR (Set []) lookup z = z
getSuccessorsR (Set (x:xs)) lookup z = if getFirst x == lookup 
					  then getSuccessorsR2 (Set (x:xs)) lookup z
					  else getSuccessorsR (Set xs) lookup z
getSuccessorsR2 (Set (x:xs)) lookup z= if inSet (getSecond x) z
					then getSuccessorsR (Set xs) lookup z 
					else getSuccessorsR (Set xs) lookup (insertSet (getSecond x) z)
					
-- returns all recursive successors (successor of successor of ... ) 
getAllSuccessors :: Ord a => Set(a,a) -> a -> Set a
getAllSuccessors x lookup = getAllSuccessorsR x lookup emptySet

getAllSuccessorsR :: Ord a => Set(a,a) -> a -> Set a -> Set a
getAllSuccessorsR (Set[]) lookup z = z 
getAllSuccessorsR (Set (x:xs)) lookup z = getSubSuccessors (Set (x:xs)) (getSuccessors (Set (x:xs)) lookup) z

getSubSuccessors :: Ord a => Set(a,a) -> Set a -> Set a -> Set a
getSubSuccessors (Set []) (Set []) z = z
getSubSuccessors (Set []) lookup z = z
getSubSuccessors x (Set []) z = z
getSubSuccessors (Set (x:xs)) (Set (y:ys)) z = getSubSuccessors (Set (x:xs)) (Set ys) (insertSet y (getAllSuccessorsR (Set (x:xs)) y z))	   


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

-- helper function : get first of tupel
getFirst :: (a,b) -> a
getFirst (a,b) = a

-- helper function : get second of tupel
getSecond :: (a,b) -> b
getSecond (a,b) = b


-- > ---- Task 5:
-------
-------------------Bug: trClosM if called e.g. with: [(1,2),(2,1)] or.  [(3,4),(4,3),(1,3)] :::: . ...
------------------- Seems to be the [(x,y),(y,x)] pattern. No further investigations  :: :: : . ....
------------------simpleAutoTest exits or not, depends on randomly generated pairs, as mentioned ^ 
-------



-- Test the function trClos from the previous exercise. Devise your own 
-- test method for this. Try to use random test generation. Define reasonable properties to test


-- Possible way: (Not implemented yet) (P. 176 shows how this could work)
--1a) Generate random Pairs/Relations [(x,y),..,(xn,yn)] and store the pairs separately.
--b) Then ask the trClos function to make transitive closure of these relations.
--c) Then check if the resulting relation is transitive.
--d) Do another check by manually going through the relations from a) and check if needed counterparts are present. 

--2) Use different algorithms, like in the following part.

-----manual
manualTest = (not (isTrans s1) &&  isTrans (trClos s1)) && not (isTransitiveClosure s1) &&  isTransitiveClosure (trClos s1)


-----automatic
simpleAutoTest :: IO Bool
simpleAutoTest = autoTest 1

--


-- maybe more complex tests with more parameters :test count, pair count [(),....()n], int element range n (x E n)
autoTest :: Int ->IO Bool
autoTest testCount  = if testCount > 0 then do 
                                               randRel <- (getRandomRelation 2)  -- relation pair count
                                               let tr = (trClos randRel)
                                               testRes <-  (autoTest (testCount -1))
                                               testResR <- (autoTestR tr)
                                               return (testRes && testResR)
                      else return True



-- 2 different proofs for transitivity 
autoTestR :: Rel Int -> IO Bool
autoTestR x = return ((isTransitiveClosure x || isTransitiveClosure (trClos x)) || ((isTrans x) || (isTrans (trClos x))))
--autoTestR2 x = return (isTransitiveClosure x || isTransitiveClosure (trClos x)) -- method #1
--autoTestR2 x = return ((isTrans x) || (isTrans (trClos x))) -- method #2



getRandomRelation ::  Int -> IO (Rel Int) 
getRandomRelation  0 = return []
getRandomRelation  n = do fstX <- getRandomInt 3 -- x range
			  fstY <- getRandomInt 3 -- y range				
			  randPairs <- getRandomRelation (n-1) 			
			  return ((fstX, fstY) : randPairs)



-- -- -- -- Transitive check method #1
-- here i can reuse the functions above:
-- to prove that a relation is a transitive closure,
-- the output of getSuccessors and getAllSuccessors 
-- must be the same for each element in the relation

isTransitiveClosure :: Ord a => Rel a -> Bool
isTransitiveClosure x = isTransitiveClosureR (relToSet x)
 
isTransitiveClosureR :: Ord a => Set (a,a) -> Bool
isTransitiveClosureR (Set []) = True
isTransitiveClosureR (Set (x:xs)) = isTransitiveIn x (Set (x:xs)) && isTransitiveClosureR (Set xs)

isTransitiveIn :: Ord a => (a,a) -> Set(a,a) -> Bool
isTransitiveIn x (Set []) = True
isTransitiveIn x (Set (y:ys)) = isEmpty(SetOperationsCK.difference (getAllSuccessors (Set (y:ys)) (getFirst x))  (getSuccessors (Set (y:ys)) (getFirst x)))



-- -- -- -- Transitive check method #2 (From book page 175. transR)
isTrans :: Ord a => Rel a -> Bool
isTrans [] = True
isTrans ( s) = and [trans pair (Set s) | pair <- s] where       
               trans (x,y) (Set r) = and [inSet (x,v) (Set r) | (u,v) <- r, u ==y]

generateTransClos ::Ord a => Rel a -> Rel a
generateTransClos rel = do trans1 <- return (nub(rel ++ (rel @@ rel))) 
                           if ((length trans1) ==(length rel))
                               then rel
                               else generateTransClos trans1

--------------------------
-- --- -- test

testT :: IO Bool
testT = do randClo <- generateRandomTransClosures
           let isT = (isTransitiveClosure randClo)
           return isT

generateRandomTransClosures :: IO (Rel Int)
generateRandomTransClosures = do randRel <- (nGenRandRel 3 2)
                                 let transClo =  (generateTransClos randRel)
                                 print transClo
                                 return transClo


nGenRandRel :: Int ->  Int -> IO (Rel Int) -- r = range; c = pairsCount
nGenRandRel r 0 = return []
nGenRandRel r c =  do randX <- getRandomInt r 
                      randY <- getRandomInt r
                      randRel <- (nGenRandRel r (c-1))
                      let noDu=  nub ((randX,randY): randRel)
                      return noDu





xnGenRandRelations :: Int -> Int  -> IO (Rel Int)-- r = range; p = pairCount
xnGenRandRelations r p  = do randRel <- nGenRandRel r p
                             print ("" ++ show randRel)
                             return randRel


-- newtype Set a = Set [a] deriving (Eq,Ord)
-- type Rel a = [(a,a)]
-- (@@) :: Eq a => Rel a -> Rel a -> Rel a