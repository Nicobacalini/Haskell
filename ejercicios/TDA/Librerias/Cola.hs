module Cola (
    Queue,
    empty, 
    isEmpty, 
    enqueue, 
    dequeue, 
    front
) where

newtype Queue a = Q [a] deriving Show

-- Crea una cola vacía
empty:: Queue a
empty = Q []

-- Verifica si la cola no tiene elementos
isEmpty:: Queue a -> Bool
isEmpty (Q []) = True
isEmpty _      = False

-- Agrega un elemento al final de la cola
enqueue:: a -> Queue a -> Queue a
enqueue a (Q xs) = Q (xs ++ [a])

-- Elimina el primer elemento de la cola
dequeue:: Queue a -> Queue a
dequeue (Q [])     = error "Error: La cola está vacía"
dequeue (Q (x:xs)) = Q xs

-- Obtiene el primer elemento sin eliminarlo
front:: Queue a -> a
front (Q [])    = error "Error: La cola está vacía"
front (Q (x:_)) = x