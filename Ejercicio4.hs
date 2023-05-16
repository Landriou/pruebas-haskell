--Ejercicio 1
suma :: [Int] -> Int
suma [] = 0
suma l = ((head l) + suma (tail l))

--Ejercicio 2
alguno :: [Bool] -> Bool
alguno l | l == [] = False
         | (head l) == True = True
         | otherwise =  alguno (tail l)

--Ejercicio 7
cuadrados :: [Float] -> [Float]
cuadrados l | l == [] = []
            | otherwise = [(head l)**2] ++ cuadrados (tail l) 

--Ejercicio 8
longList ::(Eq a) => [[a]] -> [Int]
longList l | l == [] = []
           | otherwise = [longitud (head l)] ++ longList (tail l)
longitud :: (Eq a) => [a] -> Int
longitud l | l == [] = 0
           | otherwise = 1 + longitud (tail l)

--Ejercicio 12
masDe ::(Eq a) => Int -> [[a]] -> [[a]]
masDe x l | l == [] = []
          | longitud (head l) > x = [head l] ++ masDe x (tail l)
          | otherwise = masDe x (tail l)

