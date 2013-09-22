module GenIntListCK where
import System.Random
import Control.Monad

--
-- generate a list of random integers
-- (answer taken from the internet)
--

genIntList :: IO [Int]
genIntList = genIntListR (0, 15)

genIntListR :: Random a => (a, a) -> IO [a]
genIntListR range = liftM (randomRs range) newStdGen

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))