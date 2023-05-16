--Ejercicio1
divisores :: Int -> [Int]
divisores x = filter (esDivisor x) [1..x]
esDivisor :: Int -> Int -> Bool
esDivisor x y = do if (mod x y) == 0
                     then True
                     else False

--Ejercicio3
primo :: Int -> Bool
primo x = do if (length (divisores x)) == 2
               then True
               else False

--Ejercicio 4
nPrimos :: Int -> [Int]
nPrimos x = filter primo [1..x] 

--Ejercicio 5
--No funciona por una movida de los tipos
--tomar :: Int -> [Int] ->[Int] -> [Int]
--tomar x l = do if x == 0
  --                     then head l
    --                   else ((head l) ++ (tomar (x-1) (tail l))) 

--Ejercicio 8
elemento :: Eq a => a -> [a] -> Bool
elemento e [] = False
elemento e l = do if (head l) == e
                    then True
                    else elemento e (tail l)

--Ejercicio 10
data Complejo = Complejo Float Float deriving (Show)
sumaComplejos :: Complejo -> Complejo -> Complejo
sumaComplejos (Complejo x y) (Complejo w z) = (Complejo (x+w) (z+y))

--Ejercicio 13
ocurrencias :: Int -> [Int] -> Int
ocurrencias x l = do if (l == [])
                        then 0
                        else if (head l) == x
                                then (1 + ocurrencias x (tail l))
                                else ocurrencias x (tail l)

--Ejercicio 19
reversa :: [a] -> [a]
reversa l = do if (length l) == 1
                then [head l]
                else (reversa (tail l)) ++ [head l]

