module FuncionesIntro where
import Data.Char (ord, isAlpha)

-- Ejercicio 1: Conceptos básicos y Tipos
consCinco :: Int -> Int
consCinco _ = 5

apply :: (a-> b) -> a -> b
apply f x = f x

idem :: x -> x
idem x = x

first :: (x,y) -> x
first (x,y) = x

derive :: (Floating a) => (a->a) -> a -> a -> a
derive f h x = (f(x+h)- f(x-h)) / (2*h)

pot:: (Num a, Integral b) => b -> a -> a
pot expont base = base ^ expont

xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

max3 :: (Ord a) => a -> a -> a -> a
max3 z y x = max x (max y z)

swap :: (x,y) -> (y,x)
swap (x,y) = (y,x)

-- Ejercicio 2: Orden Superior y Curryficación
cuadrado :: Int -> Int
cuadrado x = x ^ 2 

-- A
aplicarYsumar :: (Int -> Int) -> Int
aplicarYsumar f = (f 5) + (f 10)

-- B
doblecuadrado :: Int -> (Int->Int)
doblecuadrado n x = n * (cuadrado x)

-- C
dobleapliacion:: (Int -> Int) -> (Int->Int)
dobleapliacion f x = f (f x)

sumartres:: (Int -> Int) -> (Int->Int)
sumartres f x= (f x) + 3

-- D
esPar :: Int -> Bool
esPar x = x `mod` 2 == 0

-- E
andCurryficado :: Bool -> (Bool -> Bool)
andCurryficado a b = a && b

orCurryficado :: Bool -> (Bool -> Bool)
orCurryficado a b = a || b

-- F
esVocalypar :: (Int,Char) -> Bool
esVocalypar (n,c) = (n `mod` 2 == 0) && (c `elem` "aeiou")

-- Ejercicio 3: Pattern Matching y Guardas
-- Nota: 'id' ya existe en Prelude, esta es una redefinición educativa
miId :: a -> a
miId x = x

greaterGuardas :: (Ord a) => (a, a) -> Bool
greaterGuardas (x, y)
  | x > y     = True   
  | otherwise = False  

f:: (Int,Int) -> Int
f (x,y) = x 

-- Ejercicio 5: Lógica Booleana
esBisiestoLogico :: Int -> Bool
esBisiestoLogico year = (year `mod` 4 == 0) && (year `mod` 100 /= 0) || (year `mod` 400 == 0)