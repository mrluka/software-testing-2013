module FormulaGenFOL where
import Week3
import Techniques


nameList = ["x","y","z"]
--
getRandomFo :: Int -> IO [Formula]
getRandomFo n = do d <- getRandomInt (length nameList)
                   getRandomFormulas d n ["x"] -- deepth, n count

getRandomFormulas :: Int -> Int -> [Name] -> IO [Formula]
getRandomFormulas _ 0 _ = return []
getRandomFormulas d n names = do
                  f <- getRandomFormula d names
                  fs <- getRandomFormulas d (n-1) names
                  return (f:fs)

p = Atom "P"

getRandomFormula :: Int ->[Name] -> IO Formula
getRandomFormula 0 xs = if (length xs) == 0
                           then return (p [zero]) -- atom formula
                           else 
                                if (length xs) > 1
                                   then return (r (map V xs)) 
                                   else return (r [V (xs !! 0)]) 

getRandomFormula d names = do n<- getRandomInt 8
                              case n of
                                   0 -> do f <- getRandomFormula 0 names  -- Atom Name [Term]
                                           return  f

                                   1 -> do -- Eq Term Term
                                           return (Eq (V "x") (V "y")) 
                                   
                                   2 -> do fs <- getRandomFormula (d-1) names -- Neg Formula
                                           return (Neg (fs)) 

                                   3 -> do fs <- getRandomFormulas (d-1) 2 names -- Impl Formula Formula
                                           return (Impl (fs !! 0) (fs !! 1))


                                   4 -> do  -- Equi Formula Formula
                                           fs <- getRandomFormulas (d-1) 2 names 
                                           return (Equi (fs !! 0) (fs !! 1))


                                   5 -> do m <- getRandomInt 8 -- Conj [Formula]
                                           fs <- getRandomFormulas (d-1) 2 names 
                                           return (Conj fs)


                                   6 -> do m <- getRandomInt 8 -- Conj [Formula]
                                           fs <- getRandomFormula (d-1) names 
                                           return (Disj [(fs),(fs)])


                                   7 -> do m <- getRandomInt 8 -- Forall Name Formula
                                           let ad = getTermName names
                                           fs <- getRandomFormula (d-1) [ad] 
                                           return (Forall ad (fs)) 


                                   8 -> do m <- getRandomInt 8 -- Exists Name formula
                                           let ad  = (getTermName names)
                                           fs <- getRandomFormula (d-1) [ad]
                                           return (Exists ad (fs)) 
                           


getTermName:: [Name] -> Name
getTermName names | (length names) >= (length nameList) =[]
                  | otherwise =  (nameList !! ((length names))) 

