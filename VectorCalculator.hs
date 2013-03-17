--Vector Calculator

type Vector = [Int] 
type Matrix = [Vector]

--Check to make sure the matrix is valid
mtrxchk :: Matrix -> Bool
mtrxchk (x:y:xs) = if((length x) == (length y)) then
                              mtrxchk (y:xs)
                           else False
mtrxchk xs = True


--M$ has a point with this one
--This can be done using (:), but we realized this
--after it was written, and left it in as an example of haskell abstraction. 				  
indexOf :: [a] -> Int -> a
indexOf [] _ = error "Out of bounds."
indexOf (x:xs) 1 = x
indexOf (x:xs) z = indexOf xs (z-1)

--Scalar multiply
scaleMult ::(Num a) => [a] -> a -> [a]
scaleMult xs scale = scaleMultRec xs scale []

--Recursive helper function for scalar multiply
scaleMultRec :: (Num a) => [a] -> a -> [a] -> [a]
scaleMultRec [] scale acc = acc
scaleMultRec (x:xs) scale acc = scaleMultRec xs scale (acc ++ [x*scale])

--Dot Product
dotProd :: (Num a) => [a] -> [a] -> a
dotProd xs ys = if((length xs) == (length ys)) then
                  dotProdRec xs ys 0
                else error "Vectors are not the same size"

--Recursive helper fucntion for dot product
dotProdRec :: (Num a) => [a] -> [a] -> a -> a
dotProdRec [] [] acc = acc
dotProdRec (x:xs) (y:ys) acc = dotProdRec xs ys (x+y+acc)				

--Cross Product
crossProd :: (Num a) => [a] -> [a] -> [a]
crossProd xs ys = ((((indexOf xs 2)*(indexOf ys 3))-((indexOf xs 3)*(indexOf ys 2))):
                  (-(((indexOf xs 1)*(indexOf ys 3))-((indexOf xs 3)*(indexOf ys 1)))):
				  (((indexOf xs 1)*(indexOf ys 2))-((indexOf xs 2)*(indexOf ys 1))):[])

--Add two vectors 
vectorAdd :: (Num a) => [a] -> [a] -> [a]
vectorAdd xs ys = if((length xs) == (length ys)) then
                    vectorOpRec xs ys [] (+)
                    else error "Vectors are not the same size"

--Subtract two vector
vectorSub :: (Num a) => [a] -> [a] -> [a]
vectorSub xs ys = if((length xs) == (length ys)) then
                    vectorOpRec xs ys [] (-)
                    else error "Vectors are not the same size"

--Recursive helper for addition and subtraction
vectorOpRec :: (Num a) => [a] -> [a] -> [a] -> (a->a->a) -> [a]
vectorOpRec [] [] acc f = acc
vectorOpRec (x:xs) (y:ys) acc f = vectorOpRec xs ys (acc ++ [x `f` y]) f

--Get the equation of a line
lineByPoints :: (Num a) => [a] -> [a] -> ([a], [a])
lineByPoints xs ys = if((length xs) == (length ys)) then
                    case (length xs) of
                       3 -> (xs, ((indexOf ys 1) - (indexOf xs 1)): 
                       	    ((indexOf ys 2) - (indexOf xs 2)): 
                       	    ((indexOf ys 3) - (indexOf xs 3)): []) 
                       2 -> (xs, ((indexOf ys 1) - (indexOf xs 1)): 
                       	    ((indexOf ys 2) - (indexOf xs 2)): [])
                    else error "Vectors are not the same size"

--Get the equation of a line rewritten by calling vector sub. 
--The first vector in the tuple is the y-intercept, and the second is the 
--vector that is scaled by t, where l(t) = (v1, t(v2)).
lineByPoints2 :: (Num a) => [a] -> [a] -> ([a], [a])
lineByPoints2 xs ys = if((length xs) == (length ys)) then
                     (xs, vectorSub ys xs)
                     else error "Vectors are not the same size"
					
about = do putStr "Welcome to our Vecotr Calculator\n"
           putStr "Unless otherwise specified, vectors are in n dimensional space\n"
           putStr "Our functions are:\nOperation (useage)\n"
           putStr "Scalar Multiplication (scaleMult vector constant)\n"
           putStr "Dot Product (dotProd vector1 vector2)\n"
           putStr "Cross Product using vectors in 3 space (crossProd vector1 vector2)\n"
           putStr "And equation of a line by two points (lineByPoints2 vector1 vector2)\n"
		 


