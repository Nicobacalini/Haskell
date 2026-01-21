module LogicaYListas where

import Pila (Stack, emptyStk, push, pop, top, stackIsEmpty)

-- EJERCICIO 1: Validación de Paréntesis
-- Recibe un String (ej: "(()))") y devuelve True si están balanceados.
validarParentesis :: String -> Bool
validarParentesis cadena = analisis cadena emptyStk

analisis :: String -> Stack Char -> Bool
analisis [] pila = stackIsEmpty pila  -- Si terminó la cadena, la pila debe estar vacía
analisis (c:cs) pila
    | c == '('  = analisis cs (push c pila)
    | c == ')'  = not (stackIsEmpty pila) && analisis cs (pop pila)
    | otherwise = analisis cs pila -- Ignorar otros caracteres

-- EJERCICIO 2: Operaciones con Listas
-- Producto Escalar: Suma de las multiplicaciones par a par
-- Ej: [1,2] [3,4] -> 1*3 + 2*4 = 11
productoEscalar :: [Int] -> [Int] -> Int
productoEscalar xs ys = sum (zipWith (*) xs ys)

-- Puntos Fijos: Elementos cuyo valor es igual a su índice
-- Ej: [0, 10, 2] -> (0,0) y (2,2) coinciden.
puntosFijos :: [Int] -> [(Int, Int)]
puntosFijos xs = [(x,y) | (x,y) <- zip xs [0..], x == y]

-- Intersección de dos listas ORDENADAS (sin duplicados)
interseccion :: Ord a => [a] -> [a] -> [a]
interseccion [] _ = []
interseccion _ [] = []
interseccion (x:xs) (y:ys)
    | x == y    = x : interseccion xs ys
    | x < y     = interseccion xs (y:ys)
    | otherwise = interseccion (x:xs) ys

-- Zip Personalizado con condición (visto en Ej Final8)
zipSi :: (a -> b -> Bool) -> [a] -> [b] -> [(a,b)]
zipSi _ [] _ = []
zipSi _ _ [] = []
zipSi f (x:xs) (y:ys)
    | f x y     = (x,y) : zipSi f xs ys
    | otherwise = zipSi f xs ys