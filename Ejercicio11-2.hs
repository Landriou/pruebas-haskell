import Data.Array

-- Los vectores son tablas cuyos índices son números naturales.
type Vector a = Array Int a
 
-- Las matrices son tablas cuyos índices son pares de números
-- naturales. 
type Matriz a = Array (Int,Int) a

listaMatriz :: Num a => [[a]] -> Matriz a
listaMatriz xss = listArray ((1,1),(m,n)) (concat xss)
    where m = length xss
          n = length (head xss)

numFilas :: Num a => Matriz a -> Int
numFilas = fst . snd . bounds

numColumnas:: Num a => Matriz a -> Int
numColumnas = snd . snd . bounds

dimension :: Num a => Matriz a -> (Int,Int)
dimension p = (numFilas p, numColumnas p)

columnaMat :: Num a => Int -> Matriz a -> Vector a
columnaMat j p = array (1,m) [(i,p!(i,j)) | i <- [1..m]]
    where m = numFilas p

filaMat :: Num a => Int -> Matriz a -> Vector a
filaMat i p = array (1,n) [(j,p!(i,j)) | j <- [1..n]]
    where n = numColumnas p

submatriz :: Num a => Int -> Int -> Matriz a -> Matriz a
submatriz i j p = 
    array ((1,1), (m-1,n -1))
          [((k,l), p ! f k l) | k <- [1..m-1], l <- [1.. n-1]]
    where (m,n) = dimension p
          f k l | k < i  && l < j  = (k,l)
                | k >= i && l < j  = (k+1,l)
                | k < i  && l >= j = (k,l+1)
                | otherwise        = (k+1,l+1)

-- Funcion que recibe una lista de listas, en la cual cada lista representa una fila de la matriz
espiral :: Num a => [[a]] -> [a]
espiral m = filaDer (listaMatriz m)

--Funcion que corta la fila superior y la columna derecha
cortarDer :: Num a => Matriz a -> Matriz a
cortarDer m = submatriz 0 (numColumnas m) m

--Funcion que corta la fila inferior y la columna izquierda
cortarIzq :: Num a => Matriz a -> Matriz a
cortarIzq m = submatriz (numFilas m) 0 m

-- Funcion que recorre una fila de izquierda a derecha y devuelve sus valores, luego continua el recorrido
-- Se utiliza la funcion elems para extraer los elementos de los arreglos formados por filas y columnas
filaDer :: Num a => Matriz a -> [a]
--El caso base esta dado cuando solo tenemos una fila
filaDer m | (length (filaMat 1 m)) <= 1 = elems (filaMat 1 m)
          | otherwise = elems (filaMat 1 m) ++ colAbajo m

--Funcion que recorre la ultima columna y elimina la primera fila y la ultima columna
colAbajo :: Num a => Matriz a -> [a]
colAbajo m = drop 1 (elems (columnaMat (numColumnas m) m)) ++ (filaIzq (cortarDer m))

--Funcion que recorre la ultima fila de derecha a izquierda
--la funcion reverse produce que la fila se lea en la direccion deseada y no en la contraria
--para no repetir el primer valor, lo borramos de la lista resultante
filaIzq :: Num a => Matriz a -> [a]
filaIzq m = (reverse (elems (filaMat (numFilas m) m))) ++ (colArriba m)


--Funcion que recorre la primera columna de abajo hacia arriba
--se utiliza la funcion reverse para que se recorre de abajo hacia arriba y no de arriba hacia abajo
colArriba :: Num a => Matriz a -> [a]
colArriba m = drop 1 (reverse (elems (columnaMat 1 m))) ++ filaDer (cortarIzq m)

main = do { putStrLn (show (espiral [[1,2,3],[4,5,6],[7,8,9]])) }