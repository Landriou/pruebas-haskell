--Ejercicio 1
buscaCrucigrama :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigrama l pos lon ps = [x | x <- ps, (length x) == lon, match l pos x]

match :: Char -> Int -> String -> Bool
match l pos (x:xs) | ((x == l) && (pos == 1)) = True
                   | xs == "" = False
                   | otherwise = match l (pos-1) xs  