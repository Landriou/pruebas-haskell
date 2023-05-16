--Ejercicio 1
--Por comprension
--Los argumentos de la funcion definen hasta que numero queremos que llegue las listas
sumaCuadrados :: Int -> Int -> [Int]
sumaCuadrados a b = [ (x^2+y^2) | x <- [1..a], y <- [1..b]]

sumaCuadradosR :: Int -> Int -> [Int]
sumaCuadradosR a b = sumaListas ([1..a]) ([1..b])
sumaListas :: [Int] -> [Int] -> [Int]
sumaListas l s | l == [] = []
               | otherwise = [(head l)^2+(head s)^2] ++ sumaListas (tail l) (tail s)