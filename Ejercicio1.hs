--Ejercicio 1
vol :: Float -> Float
vol r = (4/3)*pi*r**(3.0)

--Falta hacer
sumaCoins :: Int -> Int -> Int -> Int -> Int -> Int
sumaCoins a b c d e = (a+b+c+d+e)

--Ejercicio 3
incrementar :: [Int] -> [Int]
incrementar t = map sumar t
--sumar :: Int -> Int
sumar a = a + 1

--Ejercicio 4
cuadrado :: Float -> Float
cuadrado a = a*a

--Ejercicio 5
cuarta :: Float -> Float
cuarta a = cuadrado(cuadrado(a))

--Ejercicio 6
media :: [Float] -> Float
media t = (sum(t))/3

--Ejercicio 7
max3 :: Int -> Int -> Int -> Int
max3 a b c = max (max a b) c

--Ejercicio 8
max6 :: Int -> Int -> Int -> Int -> Int -> Int -> Int
max6 a b c d e f = max (max3 a b c) (max3 d e f)

--Ejercicio 9
area :: Float -> Float -> Float -> Float
area a b c = sqrt(((a+b+c)/2)*(((a+b+c)/2)-a)*(((a+b+c)/2)-b)*(((a+b+c)/2)-c))

--Ejercicio 10
cuadrante :: (Float,Float) -> IO()
cuadrante (x,y) = do if x > 0
                       then if y > 0
                            then putStrLn "Estas en el primer cuadrante"
                            else putStrLn "Estas en el cuarto cuadrante" 
                       else if y > 0
                            then putStrLn "Estas en el segundo cuadrante"
                            else putStrLn "Estas en el tercer cuadrante"

--Ejercicio 11
igualesTres :: Float -> Float -> Float -> Bool
igualesTres a b c = (a == b) && (b == c)

--Ejercicio 12
diferentesTres :: Float -> Float -> Float -> Bool
diferentesTres a b c = not (a == b) && not (b == c) && not (a == c)

--Ejercicio 13
igualesCuatro :: Float -> Float -> Float -> Float -> Bool
igualesCuatro a b c d = (igualesTres a b c) && (c == d)

--Ejercicio 15
--Funcion que nos dice si un año es uniformemente divisible por 4
div4 :: Int -> Bool
div4 a = (mod a 4) == 0
--Funcion que nos dice si es uniformemente divisible por 100
div100 :: Int -> Bool
div100 a = (mod a 100) == 0
--Funcion que nos dice si es uniformemente divisible por 400
div400 :: Int -> Bool
div400 a = (mod a 400) == 0

bisiesto :: Int -> IO()
bisiesto a = do if (div4 a)
                  then if (div100 a)
				    then if (div400 a)
					       then putStrLn "Es bisiesto"
					       else putStrLn "No es bisiesto"
					    else putStrLn "Es bisiesto"
					else putStrLn "No es bisiesto"

--Ejercicio 16
xor :: Bool -> Bool -> Bool
xor a b = do if (a && b)
               then False
			else True
							 