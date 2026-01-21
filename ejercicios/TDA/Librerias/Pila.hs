module Pila (
    Stack, 
    pop,
    push,
    top,
    emptyStk,
    stackIsEmpty
) where

-- El constructor Stk no se exporta para mantener la abstracción
newtype Stack a = Stk [a] deriving Show

-- Crea una pila vacía
emptyStk :: Stack a
emptyStk = Stk []

-- Verifica si una pila está vacía
stackIsEmpty :: Stack a -> Bool
stackIsEmpty (Stk []) = True
stackIsEmpty _        = False

-- Agrega un elemento a la cima de la pila
push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)

-- Elimina el elemento en la cima de la pila
pop :: Stack a -> Stack a
pop (Stk []) = error "Error: La pila está vacía"
pop (Stk (_:xs)) = Stk xs

-- Devuelve el elemento en la cima sin sacarlo
top :: Stack a -> a
top (Stk []) = error "Error: La pila está vacía"
top (Stk (x:_)) = x