{- Lauren Hicks
hicksl3
2018-12-03
Assignment 5
-}

module Function where

	getRoot:: (Double->Double) -> Double -> Double -> Double -> Double 
	--Finds the roots of a function using the bisection method 
	getRoot f a b epsilon = if b<=a then error "b must be greater than a"
		else if (f a)*(f b)>0 then error "f(a) x f(b) must be less than zero."
		else if (f(a)==0) then a
		else if (f(b)==0) then b
		else if (f(m)==0) then m 
		else if (b-a) <= epsilon then a
		else if (((f a) * (f m))<=0) then getRoot f a m epsilon
		else if (((f b)*(f m))<0) then getRoot f m b epsilon
		else error "Error"
	    where m = (a+b)/2

	definiteIntegral:: Double -> Double -> (Double -> Double) -> Integer -> Double
	-- Calculates a definite integral using trapezoidal rule 
	definiteIntegral _ _ _ 0 = 0
	definiteIntegral a b g n = 	
		delX/2*(g a + g b + (sum[(g x)*2 | x <- [a+delX .. b-delX]]))
		where delX = (b-a)/fromIntegral n

	{- Test Cases
	
	Function: getRoot 
	Test Case Number: 1
	Input: (\x -> x^2) 1 6 0.5
	Expected Output: error
	Actual Output: error

	Function: getRoot
	Test Case Number: 2 
	Input: (\x -> x^3) -1 4 0.4
	Expected Output: -0.0625
	Actual Output: -6.25e-2

	Function: getRoot
	Test Case Number: 3 
	Input: (\x -> cos(x)) 3 6 0.01
	Expected Output: 4.71
	Actual Output: 4.7109375

	Function: definiteIntegral 
	Test Case Number: 1 
	Input: (-1) 1 (\x -> x^2) 1
	Expected Output: 2.0
	Actual Output: 2.0

	Function: definiteIntegral 
	Test Case Number: 2 
	Input: 0 (pi/2) (\x -> sin(x)) 1
	Expected Output: 0.7854
	Actual Output: 0.7853981633974483
	
	Function: definiteIntegral 
	Test Case Number: 3 
	Input: 2 8 (\x -> 5*x) 2
	Expected Output: 150
	Actual Output: 150.0

	-}	    
