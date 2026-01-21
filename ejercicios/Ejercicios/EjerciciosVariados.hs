module EjerciciosVariados where

-- COMPLEMENTARIA 1
-- Ejercicio 1
-- A
triple :: Int -> Int
triple x = x*3

-- B
second :: (a,b) -> b
second (a,b) = b 

-- C
constA:: a -> b -> a
constA a b = a

-- D (Corregido 'campose' a 'compose')
compose:: (b->c) -> (a -> b) -> a -> c
compose f g x = f (g x)

-- Ejercicio 2
-- A
letraInicial :: Int -> Char
letraInicial x
    | x == 1 = 'A'
    | x == 2 = 'B'
    | otherwise = 'x'

-- Convertir de Int a Char
codigoAChar :: Int -> Char
codigoAChar n = toEnum n

-- B
siempreCinco :: a -> Int
siempreCinco x = 5

-- D
aplicarPar :: ((a -> b), a) -> b
aplicarPar (f, x) = f x

duplicarAplicado :: ((Int -> Int), Int) -> Int
duplicarAplicado (f, x) = f x

-- Ejercicio 3
f:: Int -> Int
f x | x > 0 = x
    | otherwise = 0

esCero :: Int -> Bool
esCero x 
    | x == 0 = True
    | otherwise = False

invertirPar :: (a,b) -> (b,a)
invertirPar (x,y) = (y,x) 

esUno :: Int -> Bool
esUno x 
    | x == 1 = True
    | otherwise = False

diaValido:: Int -> Bool
diaValido x 
    | x > 0 && x < 32 = True
    | otherwise = False

esTrimestre :: Int -> Bool
esTrimestre y 
    | y > 0 && y < 4 = True
    | otherwise = False

aprobado:: Int -> [Char]
aprobado x
    | x >= 9 = "Sobresaliente"
    | x >= 7 = "Notable"
    | x >= 4 = "Aprobado"
    | otherwise = "Reprobado"

validapass:: Int -> [Char] -> Bool
validapass x y 
    | length y == x = True
    | otherwise = False

-- Ejercicio 4
longitud:: [x] -> Int 
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

concatenar :: [a] -> [a] -> [a]
concatenar [] ys = ys
concatenar xs [] = xs
concatenar (x:xs) ys = x : (concatenar xs ys) 

cuentaMay:: Int -> [Int] -> Int
cuentaMay x [] = 0
cuentaMay x (y:ys) 
    | x < y = 1 + cuentaMay x ys
    | otherwise = cuentaMay x ys

maximo :: [Int] -> Int
maximo [x] = x
maximo (x:xs)  = max x (maximo xs)

multiplos :: Int -> Int -> [Int]
multiplos x y = [ x*n | n <- [1..], x*n <= y ]

todosIguales :: [Int] -> [(Int, Bool)]
todosIguales miLista = [ (e, and [e == z | z <- miLista]) | e <- miLista ]

pitagoricas :: [(Int,Int,Int)]
pitagoricas = [ (a,b,c) | a <- [0..20], b <- [0..20], c <- [0..20], a^2 + b^2 == c^2 ]

paresDeLaSuma :: Int -> Int -> [(Int, Int)]
paresDeLaSuma n t = [ (x, y) | x <- [1..n] , y <- [1..n] , x+y == t ]

-- COMPLEMENTARIA 2
esMayorDeEdad :: Int -> Bool
esMayorDeEdad x = if x >= 18 then True else False

signoNumero:: Int -> Int 
signoNumero x 
  | x>0 = 1
  | x<0 = -1
  | otherwise = 0 

esFindesemana :: [Char] -> Bool
esFindesemana [] = False
esFindesemana lista
    | lista == "sabado" || lista == "domingo" = True
    | otherwise = False
    
esFindesemana' :: String -> Bool
esFindesemana' dia = dia `elem` ["sabado", "domingo"]

tipoDeTriangulo:: (Int,Int,Int) -> [Char]
tipoDeTriangulo (x, y, z)
    | x == y && y == z = "Equilatero"
    | x == y || y == z || x == z = "Isosceles"
    | otherwise = "Escaleno"

invertirTupla:: (a,b,c) -> (c,b,a)
invertirTupla (a,b,c) = (c,b,a)

aplicarSi :: (Int -> Int) -> (Int -> Bool) -> Int -> Int
aplicarSi f p x 
    | p x = f x       
    | otherwise = x

raices :: Double -> Double -> Double -> (Double, Double)
raices a b c = let x = (-b + sqrt(b*b - 4*a*c)) / (2*a) 
                in (x , x)


-- Ejercicio 2
sumarImpares:: [Int] -> Int
sumarImpares [] = 0
sumarImpares (x:xs) 
    | (x `mod` 2) == 0 = sumarImpares xs
    | otherwise = x + sumarImpares xs

contarElemento :: Eq a => a -> [a] -> Int
contarElemento b [] = 0 
contarElemento b (x:xs) = if x == b then 1 + contarElemento b xs else contarElemento b xs

invertirLista:: [a] -> [a]
invertirLista [] = []
invertirLista (x:xs) = invertirLista xs ++ [x]

eliminarPrimeraOcurrencia :: Eq a => a -> [a] -> [a]
eliminarPrimeraOcurrencia a [] = []
eliminarPrimeraOcurrencia a (x:xs) 
    | x == a = xs
    | otherwise = x : eliminarPrimeraOcurrencia a xs

soloIndicesPares :: [a] -> [a]
soloIndicesPares [] = []         
soloIndicesPares [x] = [x]      
soloIndicesPares (x:y:xs) = x : soloIndicesPares xs

esPalindromo :: Eq a => [a] -> Bool
esPalindromo xs = xs == invertirLista xs

-- Ejercicio 3
duplicarElementos:: [Int] -> [Int]
duplicarElementos lista = [b |a <- lista, b <- [a , a]]

obtenerPares:: [Int] -> [Int]
obtenerPares lista = [ x | x<- lista , x `mod` 2 == 0 ]

productoCartesiano1:: [a] -> [b] -> [(a,b)]
productoCartesiano1 listaA listaB = [(a,b) |a<- listaA, b<- listaB]

filtrarTuplas:: [(Int, Int)] -> [Int]
filtrarTuplas lista= [ x| (x,y)<- lista, x < y]

divisores:: Int -> [Int]
divisores n = [ x | x <- [1..n], n `mod` x == 0 ]

esPrimo:: Int -> Bool
esPrimo p = p > 1 && (length (divisores p) == 2)

numerosPrimos:: Int -> [Int]
numerosPrimos n = [ x | x <- [1..n], esPrimo x ]

matrizidentidad:: Int -> [[Int]]
matrizidentidad n = [[if i == j then 1 else 0 | j <- [1..n] ] | i <- [1..n]]

sumaDeParesDistintos:: Int -> [(Int, Int)]
sumaDeParesDistintos n = [(i,j) | j <- [1..n], i <- [1..n], i /= j ] 

-- Ejercicio 4
estaOrdenada:: [Int] -> Bool
estaOrdenada [] = True
estaOrdenada [x] = True
estaOrdenada (x:y:xs)
    | x <=y = estaOrdenada (y:xs)
    | otherwise = False

insertarEnOrden :: Ord a => a -> [a] -> [a]
insertarEnOrden y [] = [y]
insertarEnOrden y (x:xs) 
    | y < x = y : (x:xs)
    | otherwise = x : insertarEnOrden y xs

-- RECURSIÓN
eliminarDuplicados:: Eq a => [a] -> [a]
eliminarDuplicados [] = []
eliminarDuplicados [x] = [x]
eliminarDuplicados (x:y:xs) 
    | x == y = eliminarDuplicados (y:xs)
    | otherwise = x : eliminarDuplicados (y:xs)

aplanarLista :: [[a]] -> [a]
aplanarLista [] = []
aplanarLista (x:xs) = x ++ aplanarLista xs 

zipCon :: (a -> b -> c) -> [a] -> [b] -> [c]
zipCon _ [] _ = []
zipCon _ _ [] = []
zipCon f (x:xs) (y:ys) = f x y : zipCon f xs ys

prefijos :: [a] -> [[a]]
prefijos [] = []
prefijos (x:xs) = [x] : map (x:) (prefijos xs)

sublistas1 :: [a] -> [[a]]
sublistas1 [] = [[]]
sublistas1 (x:xs) = (prefijos (x:xs)) ++ (sublistas1 xs) 

esPrefijo :: Eq a => [a] -> [a] -> Bool
esPrefijo [] _ = False
esPrefijo (_:_) [] = False
esPrefijo (x:xs) (y:ys)
    | x == y = esPrefijo xs ys
    | otherwise = False

miDropWhile :: (a -> Bool) -> [a] -> [a]
miDropWhile _ [] = []
miDropWhile p (x:xs)
    | p x = miDropWhile p xs
    | otherwise = x : xs

filtrarIndices :: (Int -> Bool) -> [Int] -> [Int]
filtrarIndices f lista = aux f lista 0 
  where
    aux :: (Int -> Bool) -> [Int] -> Int -> [Int]
    aux _ [] _ = []
    aux f (x:xs) i
        | f i       = x : aux f xs (i + 1)  
        | otherwise = aux f xs (i + 1)      


