module Sol3 
where
import Data.List
import Techniques
import PermutationLH
-- import PermutationCK -- decide later which one we'll take
import TestPermutationsCK
-- import TestPermutationsLH  -- decide later which one we'll take
import GenIntListLH
--import GenIntListCK  -- let's not take this, it generates an infinite list

doCnf = do x <- getRandomForm
	   return cnf x