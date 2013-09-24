module RandomSetsCK where

import SetOrd
import Techniques

--
-- Task 2 : Implement a random data generator for the datatype Set Int
--

--
-- @todo : from Set a to Set Int ?
--

-- this is not working : error is "Couldn't match expected type `Set t0' with actual type `IO Int'"
randomSet :: Ord a => Set a
randomSet = do randomInt <- (getRandomInt 10);
		return (getRandomSet randomInt emptySet)
  
-- this is not working : error is  "Couldn't match expected type `Set t0' with actual type `IO Int'"
getRandomSet :: Ord a => Int -> Set a -> Set a
getRandomSet count set = do randomInt <- (getRandomInt 10);
			     return (getRandomSetR count randomInt set)
	 
getRandomSetR :: Ord a => Int -> a -> Set a -> Set a
getRandomSetR count newItem set = if(count > 0) 
				  then getRandomSet (count -1) (insertSet newItem set) 
				  else set