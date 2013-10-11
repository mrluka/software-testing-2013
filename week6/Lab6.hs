module Lab6

where
import Data.List
import System.Random
import Week6
import Techniques
import Text.Printf

{- 
Task 1  --------------- --------------- --------------- --------------- --------------- --------------- --------------- 1 
  Implement a function exM that does modular exponentiation of x^y 
  in polynomial time, by repeatedly squaring modulo N.
  E.g., x^33 mod 5 can be computed by means of
  x^33 (mod 5) = x^32 (mod 5) * x (mod 5)
  x^32 (mod N) is computed in five steps by means of repeatedly squaring modulo N:
  x (mod N) -> x^2 (mod N) -> x^4  (mod N) -> ...-> x^32 (mod N):
  If this explanation is too concise, look up relevant literature
-}

-- SOLUTION 1 (Source Week6.hs)
-- exM' :: Integer -> Integer -> Integer -> Integer
-- exM' _ 0 _ = 1
-- exM' x y n = let 
--               z = exM' x (y `div` 2) n
--               w = multM z z n
--                 in 
--                 if even y then w
--                 else multM x w n 


-- SOLUTION 2 (Source: http://rosettacode.org/wiki/Modular_exponentiation#Haskell)
exM' :: Integer -> Integer -> Integer -> Integer -> Integer
exM' b 0 m r = r
exM' b e m r | e `mod` 2 == 1 = exM' (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
exM' b e m r = exM' (b * b `mod` m) (e `div` 2) m r


-- SOLUTION 3 (Source: WIKI http://en.wikipedia.org/wiki/Modular_exponentiation#Memory-efficient_method)
--functional version of the algorithm found on wikipedia 
-- exM' :: Integer -> Integer -> Integer -> Integer
-- exM' base exponent modulus = exMR 0 1 base exponent modulus 

-- exMR :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
-- exMR i c base exponent modulus = if i < exponent
-- 				  then exMR (i + 1) (mod (c * base) modulus) base exponent modulus
-- 				  else c

-- SOLUTION 4 (Source: WIKI http://www.citidel.org/bitstream/10117/120/13/paper.pdf) 
-- NOT tested
-- expm :: Integer → Integer → Integer → Integer
-- expm m b k =
-- let
-- ex a k s
-- | k == 0 = s
-- | k ‘mod‘ 2 == 0 = ((ex (a∗a ‘mod‘ m)) (k ‘div‘ 2)) s
-- | otherwise = ((ex (a∗a ‘mod‘ m)) (k ‘div‘ 2)) (s∗a ‘mod‘ m)
-- in ex b k 1



-- random tests to assure that our implementation comes up with the same result as the 'regular' mod operation

-- perform 10000 random tests
doRandomTests :: IO Bool
doRandomTests = doRandomTestsR 10000

-- perform a given number of random tests
doRandomTestsR :: Integer-> IO Bool
doRandomTestsR x = 
		      if x > 0 
		      then 
		      do r <- doRandomTest
		         p <- (doRandomTestsR (x - 1))
		         return (p && r)
		      else return True

-- perform a single random test
doRandomTest :: IO Bool
doRandomTest = do  
		   x <- (getRandomInt 1000)
		   y <- (getRandomInt 1000) 
		   z <- (getRandomInt 1000) 
		   if 
		    toInteger(expM (toInteger(x+1)) (toInteger(y+1)) (toInteger(z+1)))
		      == 
		    toInteger((exM' (toInteger(x+1)) (toInteger(y+1)) (toInteger(z+1))) 1) -- Solution  2
--		    toInteger((exM' (toInteger(x+1)) (toInteger(y+1)) (toInteger(z+1)))) -- Solution 1 & 3
		   then return True
		   else error ("exM gives not the same result as expM for "	 ++ (show (x+1)) ++ " , "  ++ (show (y+1)) ++ " ," ++ (show (z+1)))

{- 
Task 2  --------------- --------------- --------------- --------------- --------------- --------------- --------------- 2 
  Check that your implementation is more efficient than expM by 
  running a number of relevant tests and documenting the results.
-}

{-
Bad news! It's slower and less efficient! :(  
--------- Christian Setting 1
*Lab6> expM 5 5000000 3			1	( 0.15 secs,   25264776 bytes)
*Lab6> Lab6.exM 5 5000000 3		1	(11.54 secs, 2459061640 bytes)

*Lab6> expM 537 5000000 319		199	( 0.74 secs,   99215856 bytes)
*Lab6> Lab6.exM 537 5000000 319 	199	(11.20 secs, 2458988504 bytes)

*Lab6> expM 537 50000000 319		23	(9.50 secs, 1053698392 bytes)
*Lab6> Lab6.exM 537 50000000 319 	*** Exception: stack overflow


--------Luka Setting 1
*Lab6> expM 5 5000000 3			1	(0.44 secs, 25210884 bytes)
*Lab6> Lab6.exM 5 5000000 3		1       (8.40 secs, 1167222876 bytes)

*Lab6> expM 537 5000000 319		199	(2.23 secs, 100806448 bytes)
*Lab6> Lab6.exM 537 5000000 319 	199	(8.32 secs, 1167228464 bytes)

*Lab6> expM 537 50000000 319		23	(27.13 secs, 1055892216 bytes)
*Lab6> Lab6.exM 537 50000000 319 	memory allocation failed (requested 2097152 bytes)

------- Luka Setting 2
Solution 1
a) exM' 5 5000000 3 = 1           | (0.01 secs, 4133996 bytes)
b) exM' 537 5000000 319 = 199     | (0.00 secs, 518112 bytes)
c) exM' 537 50000000 319 = 23     | (0.00 secs, 518344 bytes)

Solution 2 (Note: additional argument for Solution 2)
a) exM' 5 5000000 3 1 = 1         | (0.01 secs, 4662088 bytes)
b) exM' 537 5000000 319 1 = 199   | (0.00 secs, 517640 bytes)
c) exM' 537 50000000 319 1 = 23   | (0.00 secs, 517716 bytes)

Solution 3
a) exM' 5 5000000 3 = 1           | (8.85 secs, 1170832296 bytes)
b) exM' 537 5000000 319 = 199     | (8.77 secs, 1167226748 bytes)
c) exM' 537 50000000 319 = 23     | (memory allocation failed (requested 1048576 bytes))

-}


{-
Task 3  --------------- --------------- --------------- --------------- --------------- --------------- --------------- 3 
  In order to test Fermat's Primality Check (as implemented in function primeF),
  the list of prime numbers generated by Eratosthenes' sieve is useless, for Fermat's
  Primality Check correctly classify the primes as primes. Where the check can go
  wrong is on classifying composite numbers; these can slip through the Fermat test.
  Write a function composites :: [Integer] that generates the infinite list of composite 
  natural numbers. 
  Hint: modify Eratosthenes' sieve, so that instead of throwing away 
  composite numbers, it marks them as false. Next filter out the numbers marked as false.

Notes (Task 3 & Task 4)
  Test Fermat’s Primality Check (primeF) 
  Eratosthenes’ sieve is useless, it calculates primes as integers above 1 that are not multiples of primes
  Modify Eratosthenes sieve so that it marks compound integers as false 
  Fermat's Primality Check can go wrong 
  Where the check can go wrong is on classifying composite numbers; these can slip through the Fermat test.
  composites :: [Integer] should generate only prime nubers that are composite: integer above 1 that are (!) multiple of primes
  Find integer produced by composites :: [Integer] that fails the primeF test

-}
-- Source: http://www.haskell.org/haskellwiki/Prime_numbers
-- genuine yet wasteful sieve of Eratosthenes
-- primesTo m = eratos [2..m]  where
--    eratos []     = []
--    eratos (p:xs) = p : eratos (xs `minus` [p, p+p..m])
-- eratos (p:xs) = p : eratos (xs `minus` map (p*) [1..m])
-- eulers (p:xs) = p : eulers (xs `minus` map (p*) (p:xs))  
-- turner (p:xs) = p : turner [x | x <- xs, rem x p /= 0]
-- primes' = 2 : 3 : ([5,7..] `minus` unionAll [[p*p, p*p+2*p..] | p <- tail primes'])

-- minus (x:xs) (y:ys) = case (compare x y) of 
--             LT -> x : minus  xs  (y:ys)
--             EQ ->     minus  xs     ys 
--             GT ->     minus (x:xs)  ys
-- minus  xs     _     = xs

-- unionAll = foldr (\(x:xs)->(x:).union xs) []


------ Source: http://en.literateprograms.org/Sieve_of_Eratosthenes_(Haskell)#Multiples_of_primes

merge :: (Ord a) => [a] -> [a] -> [a] -- The merge function is a highly-specialized function that merges two infinite, sorted lists
merge xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : (merge xt ys)
    EQ -> x : (merge xt yt)
    GT -> y : (merge xs yt)



diff :: (Ord a) => [a] -> [a] -> [a] -- diff, in short, removes all of the elements of the second list passed to it from the first.
diff xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : (diff xt ys)
    EQ -> diff xt yt
    GT -> diff xs yt


-- primes is a list consisting of the first three primes ([2,3,5]) with all odd numbers greater than that ([7,9..]), after the list of all non-prime numbers (nonprimes) is removed, appended to it.
--primes, nonprimes :: [Integer] 
--primes    = [2, 3, 5] ++ (diff [7, 9 ..] nonprimes) 
composites :: [Integer] 
composites = foldr1 f $ map g $ tail primes -- nonprimes = foldr1 f (map g (tail primes))
  where 
    f (x:xt) ys = x : (merge xt ys)
    g p         = [ n * p | n <- [p, p + 2 ..]] --The naming of the parameter strongly implies that it is a prime. The fact that it's applied to a list of known primes confirms this. This transformation is difficult to read at first. It helps to see the type of it. The type of this transformation is:


{-
 ----- Task 4 --------------- --------------- --------------- --------------- --------------- --------------- --------------- 4 
  Use the list of composite numbers (composites :: [Integer]) to test Fermat's primality check. 
a) What is the  least composite number that you can find that fools the check, for testF k with  k = 1; 2; 3?
   
b) What happens if you increase k?
   The likelihood that the Fermat's primality check "guesses" the wrong result decreases by increasing k. (First argument of primeF).
   A high random rumber increases the reliability for the check; but increases processing-time, too.

--
Source: http://en.literateprograms.org/Sieve_of_Eratosthenes_(Haskell)#Multiples_of_primes
There is more than two orders of magnitude difference between the naive implementation's performance and the improved implementation's when picking only the ten thousandth prime! Further, this disparity increases the farther into the list of prime numbers you delve. What's going on?
--

Source: Course Slides page 21
If N is indeed prime then a^(N−1) ≡ 1 (mod N), and the test works fine.
But if N is composite, it may still happen that a^(N−1) ≡ 1 (mod N), for Fermat’s Little Theorem does not specify what happens for composite numbers . . .

Test results (Settings:  primeF 100 x, high a= 100 because less likelihood to "predict" the result wrongly)

comp_pr_test 10000000   -> 10267951 tries to fool us! False (600.72 secs, 87380096572 bytes)
comp_pr_test 10000000   -> 2508013 tries to fool us! False (66.72 secs, 11615076404 bytes)
comp_pr_test 10000000   -> 17098369 tries to fool us! (1279.73 secs, 170832519200 bytes)



-}

comp_pr_test :: Int -> IO Bool
comp_pr_test n = comp_pr_testR n composites

comp_pr_testR :: Int -> [Integer] -> IO (Bool)
comp_pr_testR 0 _ =  return True
comp_pr_testR n (x:xs) = do
                                   isPr <- primeF 3 x -- True->primeF is wrong, x is the composite of n primes, that implies it must not be a prime as itself
                                   if(isPr)
                                     then do (printf "\n %d tries to fool us! \n" x)
                                             return False
                                     else do --(printf "%d " x) -- TEST- OUTPUT can be enabled to show all checked integers, low performance ! 
                                             comp_pr_testR (n-1) xs


-- b) What happens if you increase k? TESTS
prime1Test :: Int ->  IO Bool --LOWEST likelihood to have right result (x=1)
prime1Test n = primeXTest n 1

prime2Test :: Int ->  IO Bool --likelihood factor of 2 to have right result (x=2)
prime2Test n = primeXTest n 2

prime3Test :: Int ->  IO Bool --likelihood factor of 3 to have right result (x=3)
prime3Test n = primeXTest n 3

prime10Test :: Int ->  IO Bool --likelihood factor of 10 to have right result (x=10)
prime10Test n = primeXTest n 4

primeXTest :: Int -> Int -> IO Bool --low likelihood to have right result
primeXTest 0 _= return False
primeXTest n x= do res <- primeF x 63 -- CAUTION! static integer to check whether is prime or not. 
                   if (res)
                     then return True
                     else primeXTest (n-1) x


{-
Task 5  --------------- --------------- --------------- --------------- --------------- --------------- --------------- 5
  Use the list generated by the following function for a further test of Fermat's primality check.
a)Read the entry on Carmichael numbers on Wikipedia to explain what you find.

WIKI: http://en.wikipedia.org/wiki/Carmichael_number

-> Wikipedia says that 561 is smallest Carmichael number, results (see below) show that 56052361 was often found, maybe in relation? (56052361 / 100000 = 560.5.. ~ 561)
Because of the fact that the Carmichael numbers are positive composite integers and absolute Fermat pseudoprimes, they might get identified as prime number at the first glance. 

Definition
Eine zusammengesetzte natürliche Zahl n heißt Carmichael-Zahl, falls für alle zu n teilerfremden Zahlen a die folgende Kongruenz erfüllt ist:
 a^{n-1} \equiv 1 \mod n .
561 smallest Carmichael number

Test results (Settings:  primeF 100 x, high a= 100 because less likelihood to "predict" the result wrongly)

carmi_pr_test 100   ->  118901521 tries to fool us! (0.02 secs, 3658212 bytes) (Occurred multiple times)
carmi_pr_test 100   ->  56052361 tries to fool us! (0.01 secs, 2104332 bytes) (Occurred multiple times)
carmi_pr_test 100   ->  216821881 tries to fool us! (0.03 secs, 3136508 bytes)


-}

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]


carmi_pr_test :: Int -> IO Bool
carmi_pr_test n = carmi_pr_testR n carmichael


carmi_pr_testR :: Int -> [Integer] -> IO Bool
carmi_pr_testR 0 _ =  return True
carmi_pr_testR n (x:xs) = do
                                   isPr <- primeF 100 x -- True->primeF is wrong, x is the composite of n primes, that implies it must not be a prime as itself
                                   if(isPr)
                                     then do (printf "\n %d tries to fool us! \n" x)
                                             return False
                                     else do --(printf "%d " x) -- TEST- OUTPUT can be enabled to show all checked integers, low performance ! 
                                             carmi_pr_testR (n-1) xs

{-
Task 6  --------------- --------------- --------------- --------------- --------------- --------------- --------------- 6
  Use the list from the previous exercise to test the Miller-Rabin primality check.
a) What do you find?

WIKI: http://de.wikipedia.org/wiki/Miller-Rabin-Test#Zuverl.C3.A4ssigkeit

Also the Miller-Rabin has a likelihood to give the wrong result (Monte-Carlo-Algorithm).  
After 4 steps, the chance is smaller than 0.4% to be wrong. After 10 steps already 10^(-6)

Results show: 
I)Task 5 (Carmichael) test results showed the results 118901521, 56052361, 216821881 as a prime, so does "primeF 100 216821881" most of the times! (Even with 100 as first parameter, if first parameter is 1, the result is True with only few exceptions where it is False).
The millerRabinPrimality (millerRabinPrimality 56052361 10) function exits with False with the three numbers (see above) resulted in False in all tests.
II) millerRabinTest (millerRabinPrimality 172947529 1000) returns that 172947529 is a Prime (!!!), but primeF (primeF 172947529 100) returns that it is not a Prime! 

-}



millerRabinTest :: Int -> IO Bool
millerRabinTest n = millerRabinTestR n carmichael


millerRabinTestR :: Int -> [Integer] -> IO Bool
millerRabinTestR 0 _ =  return True
millerRabinTestR n (x:xs) = do     
                              randomA <- getRandomInt 100
                              let isPr = millerRabinPrimality x 100  -- True->primeF is wrong, x is the composite of n primes, that implies it must not be a prime as itself
                              if(isPr)
                                then do (printf "\n %d tries to fool us! \n" x)
                                        return False
                              else do --(printf "%d " x) -- TEST- OUTPUT can be enabled to show all checked integers, low performance ! 
                                     millerRabinTestR (n-1) xs


-- n is the number to test; a is the (presumably randomly chosen) witness
-- Precondition of Miller-Rabin: n >= 3
millerRabinPrimality :: Integer -> Integer -> Bool
millerRabinPrimality n a
    | a <= 1 || a >= n-1 = 
        error $ "millerRabinPrimality: a out of range (" 
              ++ show a ++ " for "++ show n ++ ")" 
    | n < 2 = False
    | even n = False
    | b0 == 1 || b0 == n' = True
    | otherwise = iter (tail b)
    where
        n' = n-1
        (k,m) = find2km n'
        b0 = powMod n a m
        b = take (fromIntegral k) $ iterate (squareMod n) b0
        iter [] = False
        iter (x:xs)
            | x == 1 = False
            | x == n' = True
            | otherwise = iter xs


--

-- (eq. to) find2km (2^k * n) = (k,n)
find2km :: Integral a => a -> (a,a)
find2km n = f 0 n
    where 
        f k m
            | r == 1 = (k,m)
            | otherwise = f (k+1) q
            where (q,r) = quotRem m 2       


-- (eq. to) pow' (*) (^2) n k = n^k
pow' :: (Num a, Integral b) => (a->a->a) -> (a->a) -> a -> b -> a
pow' _ _ _ 0 = 1
pow' mul sq x' n' = f x' n' 1
    where 
        f x n y
            | n == 1 = x `mul` y
            | r == 0 = f x2 q y
            | otherwise = f x2 q (x `mul` y)
            where
                (q,r) = quotRem n 2
                x2 = sq x
 
mulMod :: Integral a => a -> a -> a -> a
mulMod a b c = (b * c) `mod` a
squareMod :: Integral a => a -> a -> a
squareMod a b = (b * b) `rem` a
 
-- (eq. to) powMod m n k = n^k `mod` m
powMod :: Integral a => a -> a -> a -> a
powMod m = pow' (mulMod m) (squareMod m)

{-
Task 7  --------------- --------------- --------------- --------------- --------------- --------------- --------------- 7
  You can use the Miller-Rabin primality check to discover some large Mersenne
  primes. The recipe: take a large prime p, and use the Miller-Rabin algorithm
  to check whether 2^p - 1 is also prime. Find information about Mersenne primes
  on internet and check whether the numbers that you found are genuine Mersenne
  primes. Report on your findings.

Following solution does not show 3 as Mersenne Prime because the millerRabinPrimality function implementation's preconditions does not accept 3 as number-to-check. To get a result from the millerRabin check, a must be greater than 1 (so at least number 2) and smaller than n-1:  a <= 1 || a >= n-1.
N is the number to check whether or not it is a Prime. a is a arbitrarily (random) number. 
3 is a Mersenne Prime, but it can not be evaluated by the mersennePrime function because it would not pass the precondition mentioned above. If n=3, then there is no possible way to pass the precondition.


Test results:
findMersennePrime
 Mersenne Prime found! ->  31 with base: 5 
 Mersenne Prime found! ->  127 with base: 7 
 Mersenne Prime found! ->  8191 with base: 13 
 Mersenne Prime found! ->  131071 with base: 17 
 Mersenne Prime found! ->  524287 with base: 19 
 Mersenne Prime found! ->  2147483647 with base: 31 
True
(0.02 secs, 5736016 bytes)

-}


findMersennePrimes :: IO Bool
findMersennePrimes = findMersennePrime (filter (>15) primes)--(filter (>3) primes) -- n > 3 because of millerRabinPrimality precondition (Wiki)

findMersennePrime :: [Integer] -> IO Bool
findMersennePrime [] =  error "Empty list in mersennePrimeR" -- should not happen :) or better: should not happen with infinite list
findMersennePrime (x:xs) = do     
                          randBaseInt <- getRandomInt (2)  -- getRandomInt (fromIntegral (x-4)) 
                          basePrim <- checkMillerRabin x -- (millerRabinPrimality x (fromIntegral(randBaseInt+2))) -- (millerRabinPrimality x  (fromIntegral(randBaseInt+2)))
                          if(basePrim)
                             then do let mersInt =   toInteger((2^x) -1)
                                     if(mersInt < 0)
                                        then return True
                                        else do randMersInt <- getRandomInt  (2) --randMersInt <- getRandomInt  (mersInt-4) 
                                                isMersPri <- checkMillerRabin mersInt -- (millerRabinPrimality (fromIntegral mersInt) (fromIntegral(randMersInt+2))) -- (fromIntegral (randMersInt+2))) 
                                                if(isMersPri)
                                                   then do (printf "\n Mersenne Prime found! ->  %d with base: %d \n" mersInt x)
                                                   else do (printf "") -- (printf " NOT %d base: %d \n" mersInt x) --findMersennePrime xs
                                                (findMersennePrime xs)     
                             else findMersennePrime xs

-- Runs checkMillerRabinPrimality X times and checks whether true or false was predominant. (Wiki page for Miller-Rabin ~: after 10 computations the chance to get the wrong result is 10^(-6).
checkMillerRabin :: Integer -> IO Bool 
checkMillerRabin p = do let allVals = (checkMillerRabinPrimality p 10)
                        let tVals  = length (filter (== True) allVals)
                        let fVals = length (filter (== False) allVals)
                        (printf "t: %d f: %d \n"  tVals fVals) -- TEST-OUTPUT:True & False count of results from computation for p
                        if((tVals > fVals))
                            then return True
                            else return False

-- n, a, res (result list, contains results of each computation if n is prime or not.)
checkMillerRabinPrimality :: Integer -> Integer -> [Bool]
checkMillerRabinPrimality _ 1 = [] -- (True).(print " das")   -- ((length (filter (==True) res)) > (length (filter (==True) res)))
checkMillerRabinPrimality n a =  (millerRabinPrimality n a) : (checkMillerRabinPrimality n (a-1))



-- 279968092772225526319680285071055534765205687154331191862498637620473983897520118172609686658950889471