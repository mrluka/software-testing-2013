module RandomSetsCK where

import SetOrd
import Techniques

--
-- Task 2 : Implement a random data generator for the datatype Set Int
--

--
-- @todo : from Set a to Set Int ?
--

randomS:: IO (Set Int)
randomS = do randomCount <- getRandomInt 10
             (randomSet randomCount)

randomSet :: Int -> IO (Set Int)
randomSet 0 = return (emptySet)
randomSet c = do randomElement <- getRandomInt 10
                 set <- (randomSet (c-1))
                 return (insertSet randomElement set)
