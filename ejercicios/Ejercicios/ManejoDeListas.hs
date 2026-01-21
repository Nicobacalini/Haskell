module ManejoDeListas where
import Data.Char (ord, isAlpha)

-- Ejercicio 6: Recursividad sobre Listas
-- A
sumaLista :: (Num a) => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

-- B
alguno :: [Bool] -> Bool
alguno [] = False
alguno (True:xs) = True
alguno (False : xs) = alguno xs

-- C
todos :: [Bool] -> Bool
todos [] = True
todos (False:_) = False
todos (True: xs) = todos xs

-- D
code :: [Char] -> [Int]
code [] = []
code (c:cs) = ord c : code cs

-- E
restos :: Int -> [Int] -> [Int]
restos divisor [] = []
restos divisor (x:xs)= (x `mod` divisor) : restos divisor xs

-- F
cuadrados :: [Int] -> [Int]
cuadrados [] = []
cuadrados (x:xs) = x*x : cuadrados xs

-- G
longitud :: [[a]] -> [Int]
longitud [] = []
longitud (l:ls) = length l : longitud ls

-- H
orden :: [(Int, Int)] -> [(Int, Int)]
orden [] = []
orden ((x,y) : xs)
  | x < 3*y = (x,y) : orden xs
  | otherwise = orden xs

-- I
pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) 
  | x `mod` 2 == 0 = x : pares xs
  | otherwise = pares xs

-- J
letras :: [Char] -> [Char]
letras [] = [] 
letras (c:cs)
  | isAlpha c = c : letras cs
  | otherwise = letras cs

-- K
masDe :: Int -> [[Int]] -> [[Int]]
masDe n [] = []
masDe n (x:xs) 
  | length x > n = x : masDe n xs
  | otherwise    = masDe n xs

-- Ejercicio 7: Listas por Comprensión (Basico)
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [ x * y | (x, y) <- zip xs ys ]

-- Ejercicio 8: Listas por Comprensión (Avanzado)
-- A
divisors :: Int -> [Int]
divisors x 
  | x <= 0 = []
  | otherwise = [ d | d <- [1..x], x `mod` d == 0]

-- B
matches :: Int -> [Int] -> [Int]
matches x xs = [d | d <- xs, x == d]

-- C
unique:: [Int] -> [Int]
unique xs = [x | (i, x) <- zip [0..] xs, not (elem x (take i xs)) ]

-- D
cuadrupla :: [(Int, Int, Int, Int)]
cuadrupla = [(a, b, c, d) | a<- [1..100], b<- [1..100], c<- [1..100], d<-[1..100], (a^2 + b^2) == (c^2 + d^2)]


-- LISTAS POR COMPRENSIÓN
-- Calcula la suma de todos los divisores propios 
sumaDivisores :: Int -> Int
sumaDivisores n = sum [d | d <- [1..n-1], n `mod` d == 0]

-- Determina si un numero es perfecto
esPerfecto n = n > 0 && n == sumaDivisores n 

todosPerfectos :: Int -> [Int]
todosPerfectos 0 = []
todosPerfectos n = [x |x<- [1..n], esPerfecto x]

vectorSuma :: [Int] -> [Int] -> [Int]
vectorSuma listaA listaB = 
    [x + y | (x, y) <- zip listaA listaB]

posicionesDe :: Eq a => a -> [a] -> [Int]
posicionesDe a lista = [y| (x, y) <- zip lista [0..] , a == x]

productoCartesiano :: [[a]] -> [[a]]
productoCartesiano [] = [[]]
productoCartesiano (x:xs) = 
    [e : p | e <- x, p <- productoCartesiano xs]

divisoresPrimos :: Int -> [Int]
divisoresPrimos 0 = []
divisoresPrimos n = [ p | p <- numerosPrimos n, n `mod` p == 0 ]