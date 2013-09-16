--UVA Software Testing 2013
--Group Di3 (Christian König, Luka Hale)
--Chapter 1 implementing exercises from "The Haskell Road" (by Jan van Eijck, Kees Doets)

module Sol2 where

import GS
import TAMO

-- > Ex 2.13 (Implement checks for the principles in Theorem 2.12) 
--1
test11 =  logEquiv1 (\ p -> not True) (\ p -> False)

test12 =  logEquiv1 (\ p -> not False) (\ p -> True)
--2
test2 = logEquiv1 (\ p -> p ==> False) (\ p -> not p)
--3
test31 = logEquiv1 (\ p -> p || True) (\ p -> True)

test32 = logEquiv1 (\ p -> p && False) (\ p -> False)
--4
test41 = logEquiv1 (\ p -> p || False) (\ p -> p)

test42 =logEquiv1 (\ p -> p && True) (\ p -> p)
--5
test5 = logEquiv1 (\ p -> p || not p) (\ p -> True)
--6
test6 = logEquiv1 (\ p -> p && not p) (\ p -> False)

-- > Ex 2.15 (Contradiction tests for propositional functions with one, two, and three variables)
contracdictionTest1 = logEquiv1 id (\ p -> p && not p)  -- P and not P
contracdictionTest2  = logEquiv2 (\ p q ->  (p && q)) (\ p q ->(p || q) && ((not p) && (not q)))
-- (p or q) and ((not p) and (not q))
contracdictionTest3  = logEquiv3 (\ p q r->  (p && q && r)) (\ p q r-> p && q && ((not p) && r))       
-- p && q && ((not p) && r)

-- > Ex 2.51 ()
-- TRUE just in case there is exactly one object among xs that satisfies p
unique :: (a -> Bool) -> [a] -> Bool
unique __ [] = error "Please provide a non-empty list"
unique p xs = length (filter (==True) (map p xs)) == 1

-- > Ex 2.52
--TRUE just in case that an even number of the xs's equals TRUE
parity :: [Bool] -> Bool
parity xs = even (length(filter (==True) xs))

-- > Ex 2.53
-- TRUE just in case an even number of the xs's have property p (Use previous parity function)
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p xs = parity (map p xs)


