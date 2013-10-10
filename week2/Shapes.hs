module Shapes where
import Week2

-- -----------------------------------------------------------------------
data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq, Show)	

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z 	| not(isTriangle 	x y z) 	= NoTriangle
		| isEquilateral 	x y z	= Equilateral
		| isIsosceles 		x y z 	= Isosceles
		| isRectangular 	x y z	= Rectangular
		| otherwise 			= Other

-- VVZ: why not sort the triple once before checking the conditions in order not to repeat combinations?
-- isTriangle tells if integers can form a triangle
isTriangle :: Integer -> Integer -> Integer -> Bool
isTriangle x y z 	= (x + y > z) && (x + z > y) && (y + z > x)

-- isEquilateral : all sides must have same length
isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral x y z 	= (x == y) && (y == z) && (x == z)

-- isIsosceles at least two side lengths must be the same
isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles x y z 	=  (x == y) || (y == z) || (x == z)

-- isRectangular : check pythagoras in any combination 
isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular x y z 	= (pythagoras x y z) || (pythagoras x z y) || (pythagoras y z x)

-- pythagoras : a well known formula : (x^2 + y^2 == z^2)
pythagoras :: Integer -> Integer -> Integer -> Bool
pythagoras x y z 	= x * x + y * y == z * z

-- VVZ: the following is still about shapes, right? Since I am reading the module called 'Shapes'...
-- -----------------------------------------------------------------------
form1NonCnf = Neg (Dsj[p,q])
form1Cnf = Cnj [Neg(p),Neg(q)]
form2NonCnf = Dsj [(Cnj [p,q]),r]
form2Cnf = Cnj [(Dsj [p,r]),(Dsj [q,r])]

testCnf :: Bool
testCnf = testCnf1 && testCnf2

testCnf1 :: Bool
testCnf1 = (cnf form1NonCnf) == form1Cnf

testCnf2 :: Bool
testCnf2 = (cnf form2NonCnf) == form2Cnf

-- no precondition : Should work on any formula
-- will take a formula, apply arrowfree, nnf and cnf in this order to the formula and return the result
-- to be honest, i have no idea if this works correctly or not
cnf :: Form -> Form
cnf x 			= cnf2 ( nnf ( arrowfree ( x )))
-- VVZ: where is the flattener? The formulae on the slides used associativity and hence assumed the flattener of x & (y & z) to x & y & z in the head of the reader, but in the implementation your rewritings could make quite a mess of the structure of conjunction/disjunction lists, not to mention that the input is 'any formula', so it can be already messed up.
-- VVZ: cnf (Cnj [Cnj [p,q,p], q])
-- VVZ: I expect to see *(1 2 1 2), not *(*(1 2 1) 2)

-- VVZ: A different counterexample: cnf (Dsj [p,p,q]) - the third operand is eaten by your algorithm

-- precondition: parameter is in nnf and arrow free
cnf2 :: Form -> Form
cnf2 (Prop x) 		= Prop x 			-- first literal case
cnf2 (Neg (Prop x)) 	= Neg (Prop x)  	-- second literal case: negation of literal
cnf2 (Cnj fs) 		= Cnj (map cnf2 fs)
cnf2 (Dsj fs) 		= dist (cnf2 (fs !! 0)) (cnf2 (fs !! 1))
-- VVZ: the line above is wrong, the type definition says that it is Dsj [Int], while you treat only two elements in a list
cnf2 x 			= error ( "form should be literal (or negation of literal), conjunction or disjunction: " ++ show x )

-- precondition: both parameters are in cnf
dist :: Form -> Form -> Form
dist (Cnj fs) y 	= Cnj [(dist (fs !! 0) y),(dist (fs !! 1) y)] --TODO find better way of writing this (using map or so)
dist x (Cnj fs) 	= Cnj [(dist x (fs !! 0)),(dist x (fs !! 1))] --TODO find better way of writing this (using map or so)
-- VVZ: how's the TODO going? ;)
dist x y 		= Dsj [x ,y]

-- VVZ: these are also shapes apparently...
-- -----------------------------------------------------------------------
-- logical contradiction
contradiction :: Form -> Bool
contradiction x 	= isContradiction (allVals x) x

isContradiction :: [Valuation] -> Form -> Bool
isContradiction [] y 	= True;
isContradiction (x:xs) y = (not (eval x y)) && (isContradiction xs y)
-- VVZ: this is kinda correct, but very un-haskell-ish, you use explicit recursion where simple mapping would suffice.
-- VVZ: something like
-- VVZ: tautology f = all (\ v -> eval v f) (allVals f)
-- VVZ: the same with others, but contradiction can be even simpler by reusing old code:
-- VVZ: contradiction = not . satisfiable

-- logical tautology
tautology :: Form -> Bool
tautology x 		= isTautology (allVals x) x
  
isTautology :: [Valuation] -> Form -> Bool
isTautology [] y 	= True
isTautology (x:xs) y 	= (eval x y) && (isTautology xs y)

--logical entailment  @TODO
entails :: Form -> Form -> Bool
entails x y 		= isEntailment x y (allVals x)

isEntailment :: Form -> Form -> [Valuation] -> Bool
isEntailment x y [] = True
isEntailment x y (z:zs) = (((((eval z x) == True && (eval z y) == True) || ((eval z x) == False))) && isEntailment x y zs)	
-- VVZ: wow, there has GOT to be a shorter way of writing it.

-- logical equivalence 
equiv :: Form -> Form -> Bool
equiv x y 		= isEquiv x y (allVals x)

isEquiv :: Form -> Form -> [Valuation] -> Bool
isEquiv x y [] 		= True
isEquiv x y (z:zs) 	= ((eval z x) == (eval z y)) && (isEquiv x y zs)
-- VVZ: again, could be written much simpler as 'entails x y && entails y x'
-- VVZ: or even 'tautology (Equiv x y)'
-- VVZ: be lazy, reuse your own functions!
