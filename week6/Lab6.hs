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
-->> At the end of this file is a complete listing of all found Mersenne Primes
True
(0.02 secs, 5736016 bytes)

-}


findMersennePrimes :: IO Bool
findMersennePrimes = findMersennePrime (filter (>15) primes)--(filter (>3) primes) -- n > 3 because of millerRabinPrimality precondition (Wiki)

findMersennePrime :: [Integer] -> IO Bool
findMersennePrime [] =  error "Empty list in mersennePrimeR" -- should not happen :) or better: should not happen with infinite list
findMersennePrime (x:xs) = do     
--                          randBaseInt <- getRandomInt (2)  -- getRandomInt (fromIntegral (x-4)) 
                          basePrim <- checkMillerRabin x -- (millerRabinPrimality x (fromIntegral(randBaseInt+2))) -- (millerRabinPrimality x  (fromIntegral(randBaseInt+2)))
                          if(basePrim)
                             then do let mersInt =   toInteger((2^x) -1)
                                     if(mersInt < 0)
                                        then return True
                                        else do -- randMersInt <- getRandomInt  (2) --randMersInt <- getRandomInt  (mersInt-4) 
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
                      --  (printf "t: %d f: %d \n"  tVals fVals) -- TEST-OUTPUT:True & False count of results from computation for p
                        if((tVals > fVals))
                            then return True
                            else return False

-- n, a
checkMillerRabinPrimality :: Integer -> Integer -> [Bool]
checkMillerRabinPrimality _ 1 = [] 
checkMillerRabinPrimality n a =  (millerRabinPrimality n a) : (checkMillerRabinPrimality n (a-1))



{-

Mersenne Prime found! ->  131071 with base: 17 

 Mersenne Prime found! ->  524287 with base: 19 

 Mersenne Prime found! ->  2147483647 with base: 31 

 Mersenne Prime found! ->  2305843009213693951 with base: 61 

 Mersenne Prime found! ->  618970019642690137449562111 with base: 89 

 Mersenne Prime found! ->  162259276829213363391578010288127 with base: 107 

 Mersenne Prime found! ->  170141183460469231731687303715884105727 with base: 127 

 Mersenne Prime found! ->  6864797660130609714981900799081393217269435300143305409394463459185543183397656052122559640661454554977296311391480858037121987999716643812574028291115057151 with base: 521 

 Mersenne Prime found! ->  531137992816767098689588206552468627329593117727031923199444138200403559860852242739162502265229285668889329486246501015346579337652707239409519978766587351943831270835393219031728127 with base: 607 

 Mersenne Prime found! ->  10407932194664399081925240327364085538615262247266704805319112350403608059673360298012239441732324184842421613954281007791383566248323464908139906605677320762924129509389220345773183349661583550472959420547689811211693677147548478866962501384438260291732348885311160828538416585028255604666224831890918801847068222203140521026698435488732958028878050869736186900714720710555703168729087 with base: 1279 

 Mersenne Prime found! ->  1475979915214180235084898622737381736312066145333169775147771216478570297878078949377407337049389289382748507531496480477281264838760259191814463365330269540496961201113430156902396093989090226259326935025281409614983499388222831448598601834318536230923772641390209490231836446899608210795482963763094236630945410832793769905399982457186322944729636418890623372171723742105636440368218459649632948538696905872650486914434637457507280441823676813517852099348660847172579408422316678097670224011990280170474894487426924742108823536808485072502240519452587542875349976558572670229633962575212637477897785501552646522609988869914013540483809865681250419497686697771007 with base: 2203 

 Mersenne Prime found! ->  446087557183758429571151706402101809886208632412859901111991219963404685792820473369112545269003989026153245931124316702395758705693679364790903497461147071065254193353938124978226307947312410798874869040070279328428810311754844108094878252494866760969586998128982645877596028979171536962503068429617331702184750324583009171832104916050157628886606372145501702225925125224076829605427173573964812995250569412480720738476855293681666712844831190877620606786663862190240118570736831901886479225810414714078935386562497968178729127629594924411960961386713946279899275006954917139758796061223803393537381034666494402951052059047968693255388647930440925104186817009640171764133172418132836351 with base: 2281 

 Mersenne Prime found! ->  259117086013202627776246767922441530941818887553125427303974923161874019266586362086201209516800483406550695241733194177441689509238807017410377709597512042313066624082916353517952311186154862265604547691127595848775610568757931191017711408826252153849035830401185072116424747461823031471398340229288074545677907941037288235820705892351068433882986888616658650280927692080339605869308790500409503709875902119018371991620994002568935113136548829739112656797303241986517250116412703509705427773477972349821676443446668383119322540099648994051790241624056519054483690809616061625743042361721863339415852426431208737266591962061753535748892894599629195183082621860853400937932839420261866586142503251450773096274235376822938649407127700846077124211823080804139298087057504713825264571448379371125032081826126566649084251699453951887789613650248405739378594599444335231188280123660406262468609212150349937584782292237144339628858485938215738821232393687046160677362909315071 with base: 3217 

 Mersenne Prime found! ->  190797007524439073807468042969529173669356994749940177394741882673528979787005053706368049835514900244303495954950709725762186311224148828811920216904542206960744666169364221195289538436845390250168663932838805192055137154390912666527533007309292687539092257043362517857366624699975402375462954490293259233303137330643531556539739921926201438606439020075174723029056838272505051571967594608350063404495977660656269020823960825567012344189908927956646011998057988548630107637380993519826582389781888135705408653045219655801758081251164080554609057468028203308718724654081055323215860189611391296030471108443146745671967766308925858547271507311563765171008318248647110097614890313562856541784154881743146033909602737947385055355960331855614540900081456378659068370317267696980001187750995491090350108417050917991562167972281070161305972518044872048331306383715094854938415738549894606070722584737978176686422134354526989443028353644037187375385397838259511833166416134323695660367676897722287918773420968982326089026150031515424165462111337527431154890666327374921446276833564519776797633875503548665093914556482031482248883127023777039667707976559857333357013727342079099064400455741830654320379350833236245819348824064783585692924881021978332974949906122664421376034687815350484991 with base: 4253 

 Mersenne Prime found! ->  285542542228279613901563566102164008326164238644702889199247456602284400390600653875954571505539843239754513915896150297878399377056071435169747221107988791198200988477531339214282772016059009904586686254989084815735422480409022344297588352526004383890632616124076317387416881148592486188361873904175783145696016919574390765598280188599035578448591077683677175520434074287726578006266759615970759521327828555662781678385691581844436444812511562428136742490459363212810180276096088111401003377570363545725120924073646921576797146199387619296560302680261790118132925012323046444438622308877924609373773012481681672424493674474488537770155783006880852648161513067144814790288366664062257274665275787127374649231096375001170901890786263324619578795731425693805073056119677580338084333381987500902968831935913095269821311141322393356490178488728982288156282600813831296143663845945431144043753821542871277745606447858564159213328443580206422714694913091762716447041689678070096773590429808909616750452927258000843500344831628297089902728649981994387647234574276263729694848304750917174186181130688518792748622612293341368928056634384466646326572476167275660839105650528975713899320211121495795311427946254553305387067821067601768750977866100460014602138408448021225053689054793742003095722096732954750721718115531871310231057902608580607 with base: 4423 
-}