module TransitiveClosureCK where

import SetOrd
import Data.List
import SetOperationsCK
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
-- Use this to implement a function rClos :: Ord a => Rel a -> Rel a 
-- that gives the transitive closure of a relation, where the relation is
-- represented as a list of pairs.
-- E.g., trClos [(1,2),(2,3),(3,4)] should give
-- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].
-- 

s1 = [(1,2),(2,3),(3,4),(4,5)]
s2 = [(3,1),(3,5),(5,9),(1,9)]
s3 = [(1,2),(2,3)]

-- @todo cleanup implementation
rClos :: Ord a => Rel a -> Rel a
rClos x = setToRel ( rClos1 ( relToSet x ) )

rClos1 :: Ord a => Set (a,a) -> Set (a,a)
rClos1 x = x `SetOperationsCK.union` rClos2 x emptySet

rClos2 :: Ord a => Set(a,a) -> Set(a,a) -> Set(a,a)
rClos2 (Set []) y = y
rClos2 (Set (x:xs)) y = rClos2 (Set xs) (rClos3 (Set (x:xs)) x y )

rClos3 (Set []) current z = z
rClos3 (Set (x:xs)) current z = rClos4 current (getAllSuccessors (Set (x:xs)) (getFirst x)) z

rClos4 current (Set []) z = z
rClos4 current (Set (y:ys)) z = rClos4 current (Set ys) (rClos5 current y z)

rClos5 current y z =  if inSet (getFirst current,y) z
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


-- Task 5:
-- Test the function trClos from the previous exercise. Devise your own 
-- test method for this. Try to use random test generation. Define reasonable properties to test
--

-- here i can reuse the functions above:
-- to prove that a relation is a transitive closure,
-- the output of getSuccessors and getAllSuccessors 
-- must be the same for each element in the relation
--
isTransitiveClosure :: Ord a => Rel a -> Bool
isTransitiveClosure x = isTransitiveClosureR (relToSet x)

isTransitiveClosureR :: Ord a => Set (a,a) -> Bool
isTransitiveClosureR (Set []) = True
isTransitiveClosureR (Set (x:xs)) = isTransitiveIn x (Set (x:xs)) && isTransitiveClosureR (Set xs)

isTransitiveIn :: Ord a => (a,a) -> Set(a,a) -> Bool
isTransitiveIn x (Set []) = True
isTransitiveIn x (Set (y:ys)) = isEmpty(SetOperationsCK.difference (getAllSuccessors (Set (y:ys)) (getFirst x))  (getSuccessors (Set (y:ys)) (getFirst x)))

manualTest = not (isTransitiveClosure s1) &&  isTransitiveClosure (rClos s1)

autoTest :: Bool
autoTest = autoTestR 1000

autoTestR x = if x > 0 then autoTestR (x -1) && autoTestR2 getRandomRelation else True

-- @todo replace with real randomizer
getRandomRelation :: Ord a => Num a=> Rel a
getRandomRelation = setToRel (insertSet (1,2) ( insertSet (2,3) (insertSet (3,4) emptySet)))

autoTestR2 :: Ord a => Rel a -> Bool
autoTestR2 x = isTransitiveClosure x || isTransitiveClosure (rClos x)