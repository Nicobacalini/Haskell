module Ordenamiento where

-- QuickSort (O(n log n))
-- Función auxiliar para dividir la lista
particion :: Ord a => a -> [a] -> ([a],[a])
particion _ [] = ([],[])
particion pivote (x:xs) 
    | x < pivote = (x:menores, mayores)
    | otherwise  = (menores, x:mayores)
    where (menores, mayores) = particion pivote xs

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (pivote:xs) = quickSort izquierda ++ [pivote] ++ quickSort derecha
    where (izquierda, derecha) = particion pivote xs

-- MergeSort (O(n log n))
-- Mezcla dos listas ordenadas en una sola
mezclar :: Ord a => [a] -> [a] -> [a]
mezclar xs [] = xs
mezclar [] ys = ys
mezclar (x:xs) (y:ys) 
    | x <= y    = x : mezclar xs (y:ys)
    | otherwise = y : mezclar (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = [] 
mergeSort [x] = [x]
mergeSort xs = mezclar (mergeSort primeraMitad) (mergeSort segundaMitad)
    where 
        mitad = length xs `div` 2
        (primeraMitad, segundaMitad) = splitAt mitad xs

-- Selection Sort (O(n^2))
minimoYResto :: Ord a => [a] -> (a, [a])
minimoYResto [x] = (x, []) 
minimoYResto (x:xs) = 
    let (minCola, restoCola) = minimoYResto xs 
    in if x <= minCola                         
        then (x, xs)                              
        else (minCola, x : restoCola)

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = 
    let (m, resto) = minimoYResto xs
    in m : selectionSort resto

-- Insertion Sort (O(n^2))
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
    | x <= y    = x : (y:ys)
    | otherwise = y : insert x ys

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

-- Bubble Sort (O(n^2)) - Método Burbujeo
-- Realiza una pasada de burbujeo intercambiando adyacentes
burbujeo :: Ord a => [a] -> ([a], Bool)
burbujeo [] = ([], False)
burbujeo [x] = ([x], False)
burbujeo (x:y:xs)
    | x > y     = let (res, _) = burbujeo (x:xs) in (y:res, True) -- Hubo cambio
    | otherwise = let (res, c) = burbujeo (y:xs) in (x:res, c)    -- No hubo cambio aquí

-- Repite el burbujeo hasta que no haya más cambios
bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = 
    let (listaNueva, huboCambio) = burbujeo xs
    in if huboCambio 
       then bubbleSort listaNueva 
       else listaNueva

-- UTILIDADES
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) 
    | x <= y = isSorted (y:xs)
    | otherwise = False

delete1 :: Eq a => a -> [a] -> [a]
delete1 a [] = []
delete1 a (x:xs) 
    | a /= x = x : delete1 a xs
    | otherwise = xs

isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] [x] = False
isPermutation [x] [] = False
isPermutation (x:xs) (y:ys) 
    | x == y = isPermutation xs ys
    | otherwise = False

minimoGeneral :: Ord a => [a] -> a
minimoGeneral [x] = x
minimoGeneral (x:xs) = min x (minimoGeneral xs)