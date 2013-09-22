module GenIntListLH where
import Techniques
-- > --------- Ex 3
-- Consult the course slides of this week to write a generator for random integer lists. 
-- The type should be: genIntList :: IO [Int]

genIntList :: IO [Int]
genIntList = do d <- getRandomInt 20
                genIntListHelper d 10 -- random <boundaries>, fixed (10) length 
--                genIntListHelper d d -- list <length> same as random <boundaries>
--                genIntListHelper 10 d -- random <length>, fixed <boundaries> (10) 

-- #1: d- random Int boundaries (domain?)
-- #2: n- counter (list length) 
genIntListHelper :: Int -> Int -> IO [Int]
genIntListHelper _ 0 = return []
genIntListHelper d n = do 
                     f <- getRandomInt d
                     fs <- genIntListHelper d (n-1) 
                     return (f:fs)