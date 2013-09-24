module SetOperationsCK where

import SetOrd
import RandomSetsCK
-- 
-- Task 3 : Implement operations for set intersection, set union and set difference
-- Next, use automated random testing to check that your implementation is correct.
--


intersection :: Ord a => Set a -> Set a -> Set a
intersection x y = doIntersection x y emptySet

doIntersection :: Ord a => Set a -> Set a -> Set a -> Set a
doIntersection (Set []) (Set []) z = z
doIntersection x (Set []) z = z
doIntersection (Set []) y z = z
doIntersection (Set (x:xs)) (Set(y:ys)) z =  if inSet x (Set (y:ys)) 
					      then doIntersection (Set xs) (Set (y:ys)) (insertSet x z) 
					      else doIntersection (Set xs) (Set (y:ys)) z

union :: Ord a => Set a -> Set a -> Set a
union x y = unionR y (unionR x emptySet)

unionR :: Ord a => Set a -> Set a -> Set a
unionR (Set []) z = z
unionR (Set (x:xs)) z = if inSet x z
			 then unionR (Set xs) z 
			 else unionR (Set xs) (insertSet x z)

difference :: Ord a => Set a -> Set a -> Set a
difference x y = differenceR x y (differenceR y x emptySet)

differenceR :: Ord a=> Set a -> Set a -> Set a -> Set a
differenceR (Set []) y z = z
differenceR x (Set []) z = z
differenceR (Set (x:xs)) y z = if inSet x y
			      then differenceR (Set xs) y z 
			      else differenceR (Set xs) y (insertSet x z)

