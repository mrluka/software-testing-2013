module FormulaGenFOL where
import Week3
import Techniques


nameList = [x,y,z,zero]
--
getRandomFo :: Int -> IO [Formula]
getRandomFo n = do d <- getRandomInt (length nameList)
                   getRandomFormulas d n [] -- [(nameList !! 0)] -- deepth, n count

getRandomFormulas :: Int -> Int -> [Term] -> IO [Formula]
getRandomFormulas _ 0 _ = return []
getRandomFormulas d n names = do
                  f <- getRandomFormula d names
                  fs <- getRandomFormulas d (n-1) names
                  return (f:fs)

p = Atom "P"

-- > Atomic: 
--           NO bound variables, only free. IFF only in this actomic formula!
-- >Negation:
--            x is free in \negφ if and only if x is free in φ. x is bound in \negφ if and only if x is bound in φ.
-- > Binary connectives:
                        -- x is free in (φ \rightarrow ψ) if and only if x is free in either φ or ψ. x is bound in (φ \rightarrow ψ) if and only if x is bound in either φ or ψ. The same rule applies to any other binary connective in place of \rightarrow.
-- > Quantifiers:
                -- x is free in \forally φ if and only if x is free in φ and x is a different symbol from y. Also, x is bound in \forally φ if and only if x is y or x is bound in φ. The same rule holds with \exists in place of \forall.

getRandomFormula :: Int ->[Term] -> IO Formula
getRandomFormula 0 xs = if (length xs) == 0
                           then do let atName = (getTermName xs)
                                   randomTermInt <- (getRandomInt 1)
                                   if (randomTermInt == 0)
                                      then return (r [z])
                                      else return (r [z, atName])
                           else  return (r (xs))

getRandomFormula d names = do n<- getRandomInt 7
                              case n of
--0 -> do f <- getRandomFormula 0 names  -- Atom Name [Term]
--return  f

                                   0 -> do -- Eq Term Term
                                           return (Eq (V "x") (V "y")) 
                                   
                                   1 -> do fs <- getRandomFormula (d-1) names -- Neg Formula
                                           return (Neg (fs)) 

                                   2 -> do f1 <- getRandomFormula (d-1) names -- Impl Formula Formula
                                           let unboundV = (getTermName names)
                                           f2 <- getRandomFormula (d-1) (reverse (unboundV:names))
                                           return (Impl f1 f2)

                                   3 -> do f1 <- getRandomFormula (d-1) names -- Equi Formula Formula
                                           let unboundV = (getTermName names)
                                           f2 <- getRandomFormula (d-1) (reverse (unboundV:names))
                                           return (Equi f1 f2)
                                            
                                   4 -> do f1 <- getRandomFormula (d-1) names -- Conj [Formula] 
                                           let unboundV = (getTermName names)
                                           f2 <- getRandomFormula (d-1) (reverse (unboundV:names))
                                           return (Conj [f1,f2])

                                   5 -> do f1 <- getRandomFormula (d-1) names -- Conj [Formula]
                                           f2 <- getRandomFormula (d-1) (reverse names) 
                                           return (Disj [(f1),(f2)])

                                   6 -> do m <- getRandomInt 8 -- Forall Name Formula
                                           let ad = getTermName names
                                           fs <- getRandomFormula (d-1) (ad:names) 
                                           return (Forall  ((varsInTerm ad) !!0) (fs)) 

                                   7 -> do m <- getRandomInt 8 -- Exists Name formula
                                           let ad  = (getTermName names)
                                           fs <- getRandomFormula (d-1) (ad:names) 
                                           return (Exists ((varsInTerm ad) !!0) (fs)) 
                           

getTermName:: [Term] -> Term
getTermName names | (length names) >= (length nameList) = zero
                  | otherwise =  (nameList !! ((length names))) 

