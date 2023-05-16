main = do { putStrLn (show (nPrimos 90)) }

divisores :: Integral a => a -> [a]
divisores n = [n | y <- [1..n], n <- [y | (mod n y) == 0]]

nPrimos :: Int -> [Int]
nPrimos n = [x | x <- [1..n], (divisores x) == [1,x]]