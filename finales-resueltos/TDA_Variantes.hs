module TDA_Variantes where

-- 1: SET (Conjunto) con Listas
newtype Set a = S [a] deriving Show

emptySet :: Set a
emptySet = S []

inSet :: (Eq a) => a -> Set a -> Bool
inSet x (S xs) = elem x xs

addSet :: (Eq a) => a -> Set a -> Set a
addSet x (S xs)
    | inSet x (S xs) = S xs
    | otherwise      = S (x:xs)

delSet :: (Eq a) => a -> Set a -> Set a
delSet x (S xs) = S (filter (/= x) xs)

unionSet :: (Eq a) => Set a -> Set a -> Set a
unionSet (S xs) (S ys) = S (foldr (\x acc -> if elem x acc then acc else x:acc) ys xs)

-- 2: COLA DE PRIORIDAD (Heap Sort)
-- Implementación simple usando listas ordenadas para simular Priority Queue
newtype PQ a = P [a] deriving Show

emptyPQ :: PQ a
emptyPQ = P []

-- Inserta ordenado (Menor a Mayor)
insertPQ :: (Ord a) => a -> PQ a -> PQ a
insertPQ x (P xs) = P (insertarOrdenado x xs)
  where
    insertarOrdenado e [] = [e]
    insertarOrdenado e (l:ls)
        | e <= l    = e : l : ls
        | otherwise = l : insertarOrdenado e ls

-- Obtiene el mínimo (el primero de la lista ordenada)
nextPQ :: PQ a -> a
nextPQ (P [])    = error "Cola vacia"
nextPQ (P (x:_)) = x

-- Elimina el mínimo
deleteMinPQ :: PQ a -> PQ a
deleteMinPQ (P [])     = error "Cola vacia"
deleteMinPQ (P (_:xs)) = P xs

-- Algoritmo HeapSort (usando la PQ)
heapSort :: (Ord a) => [a] -> [a]
heapSort xs = vaciarPQ (llenarPQ xs emptyPQ)

llenarPQ :: (Ord a) => [a] -> PQ a -> PQ a
llenarPQ [] pq     = pq
llenarPQ (x:xs) pq = llenarPQ xs (insertPQ x pq)

vaciarPQ :: PQ a -> [a]
vaciarPQ (P []) = []
vaciarPQ pq     = nextPQ pq : vaciarPQ (deleteMinPQ pq)