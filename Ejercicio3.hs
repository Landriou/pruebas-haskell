--Ejercicio 1
sumaPares :: [(Int,Int)] -> [Int]
sumaPares [] = []
sumaPares l = [sumaPar(head l)] ++ sumaPares (tail l)
--Funcion encargada de sumar cada uno de los pares
sumaPar :: (Int,Int) -> Int
sumaPar (a,b) = a+b

--Ejercicio 3
zipSort :: [Int] -> [Int] -> [(Int,Int)]
zipSort [] [] = []
zipSort l1 l2 = [(maxMin (head l1) (head l2))] ++ (zipSort (tail l1) (tail l2))
maxMin :: Int -> Int -> (Int,Int)
maxMin h1 h2 = (min h1 h2,max h1 h2)

--Ejercicio 4

--data Date = Date Int Int Int deriving (Eq)
--data Persona = Persona String String Date
--Rehacer
--takePersonas :: Date -> [Persona] -> [Persona]
--takePersonas (Date x w z) [(Persona n a (Date y m d))] = filter (compFecha (Date x w z) (Date y m d)) [(Persona n a (Date y m d))] 
--Funcion que devuelve true si la fecha 2 es mayor que la 1
--compFecha :: Date -> Date -> Bool
--compFecha (Date a b c) (Date x y z) = do if (a < x)
  --                                         then True
    --                                       else if(a == x)
      --                                          then if(b < y)
        --                                            then True
          --                                          else if(b == y)
            --                                            then if(c <= z)
              --                                              then True
                --                                            else False
                  --                                      else False
                    --                            else False 

--Ejercicio 7
data Notas =  Uno | Dos | Tres | Cuatro | Cinco | Seis | Siete | Ocho | Nueve | Diez
nota :: Float -> Int
nota x | x < 10 = 0
       | x < 20 = 1
       | x < 30 = 2
       | x < 40 = 3
       | x < 50 = 4
       | x < 60 = 5
       | x < 70 = 6
       | x < 80 = 7
       | x < 90 = 8
       | x < 100 = 9
       | x == 100 = 10
notaP :: Float -> Int
notaP x = round (x / 10)
