module TransitiveClosureCK where

import Data.List
--
-- Task 4:
-- Suppose we implement binary relations as list of pairs, Haskell type [(a,a)].
-- Assume the following defnitions:
-- 

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- @todo implementation
rClos :: Ord a => Rel a -> Rel a
rClos x = x

--
-- Test the function trClos from the previous exercise. Devise your own 
-- test method for this. Try to use random test generation. Define reasonable properties to test
--