module AritmeticaBinaria where

-- Ejercicio 10: Aritmética Binaria y Tipos de Datos
data DigBin = Cero | Uno deriving (Show, Eq)

sumaM2 :: DigBin -> DigBin -> DigBin
sumaM2 Cero Cero = Cero
sumaM2 Uno Cero = Uno
sumaM2 Cero Uno = Uno
sumaM2 Uno Uno = Cero

prodM2 :: DigBin -> DigBin -> DigBin
prodM2 Cero Cero = Cero
prodM2 Uno Cero = Cero
prodM2 Cero Uno = Cero
prodM2 Uno Uno = Uno

type NumBin = [DigBin]

restoDos :: NumBin -> DigBin
restoDos [] = Cero
restoDos (x:xs) = x

cocDos :: NumBin -> NumBin
cocDos []     = []
cocDos (x:xs) = xs

prodDos :: NumBin -> NumBin
prodDos xs = Cero : xs

sumBin :: NumBin -> NumBin -> NumBin
sumBin xs ys = sumAux xs ys Cero

sumAux :: NumBin -> NumBin -> DigBin -> NumBin
sumAux [] [] Cero = []
sumAux [] [] Uno  = [Uno]
sumAux [] (y:ys) c = d_out : sumAux [] ys c_new
    where 
        d_out = sumaM2 y c
        c_new = prodM2 y c
sumAux (x:xs) [] c = d_out : sumAux xs [] c_new
    where
        d_out = sumaM2 x c
        c_new = prodM2 x c
sumAux (x:xs) (y:ys) c = d_out : sumAux xs ys c_new
    where 
        s_xy  = sumaM2 x y
        d_out = sumaM2 s_xy c
        c_new = sumaM2 (prodM2 x y) (prodM2 c s_xy)

-- Resto de la division por dos
restoDosNC :: NumBin -> DigBin
restoDosNC []       = Cero
restoDosNC [x]      = x  
restoDosNC (x:xs) = restoDosNC xs 

-- Cociente de la division por dos 
cocDosNC :: NumBin -> NumBin
cocDosNC []     = []
cocDosNC [x]    = []
cocDosNC (x:xs) = x : cocDosNC xs

-- Producto por dos
prodDosNC :: NumBin -> NumBin
prodDosNC xs = xs ++ [Cero]

sumBinNC :: NumBin -> NumBin -> NumBin
sumBinNC xs ys = reverse (sumBin (reverse xs) (reverse ys))

-- Multiplicacion Convencion
multBinOC :: NumBin -> NumBin -> NumBin
multBinOC xs [] = []
multBinOC xs (y:ys) = sumBin term_actual term_recurrente
    where
        -- Término actual (xs * y)
        term_actual
            | y == Uno  = xs
            | otherwise = [] -- xs * Cero = Cero (representado por [])
        term_recurrente = prodDos (multBinOC xs ys)

-- Multiplicacion Convencion Opuesta
multBinNC :: NumBin -> NumBin -> NumBin
multBinNC xs ys = reverse (multBinOC (reverse xs) (reverse ys))