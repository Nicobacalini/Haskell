module ArbolBinario(
    Arbol,
    vacio,
    insertar,
    eliminar,
    esVacio,
    altura,
    cantidadNodos,
    buscar,
    inOrder,
    preOrder,
    postOrder,
    espejo
) where

data Arbol a = Hoja | Nodo a (Arbol a) (Arbol a) deriving Show

-- Crea un árbol vacío
vacio:: Arbol a 
vacio = Hoja

esVacio:: Arbol a -> Bool
esVacio Hoja = True
esVacio _    = False

-- Inserta un elemento manteniendo la propiedad de árbol binario
insertar :: Ord a => a -> Arbol a -> Arbol a
insertar x Hoja = Nodo x Hoja Hoja
insertar x (Nodo y izq der) 
    | x == y = Nodo y izq der              -- No duplicados
    | x > y  = Nodo y izq (insertar x der) -- Mayores a la derecha
    | otherwise = Nodo y (insertar x izq) der -- Menores a la izquierda

-- ALGORITMOS DE BORRADO (Complejo)
-- Elimina un elemento del árbol manteniendo la estructura BST
eliminar :: Ord a => a -> Arbol a -> Arbol a
eliminar _ Hoja = Hoja
eliminar x (Nodo y izq der)
    | x < y     = Nodo y (eliminar x izq) der
    | x > y     = Nodo y izq (eliminar x der)
    | otherwise = borrarRaiz (Nodo y izq der) -- Encontramos el nodo a borrar

-- Función auxiliar para manejar los casos de borrado (0, 1 o 2 hijos)
borrarRaiz :: Ord a => Arbol a -> Arbol a
borrarRaiz (Nodo _ Hoja der) = der  -- Caso 1: Solo hijo derecho (o hoja)
borrarRaiz (Nodo _ izq Hoja) = izq  -- Caso 2: Solo hijo izquierdo
borrarRaiz (Nodo _ izq der)  =      -- Caso 3: Dos hijos
    let (minimo, nuevaDer) = extraerMinimo der -- Buscamos el menor del lado derecho
    in Nodo minimo izq nuevaDer

-- Extrae el elemento más pequeño de un árbol
extraerMinimo :: Arbol a -> (a, Arbol a)
extraerMinimo (Nodo x Hoja der) = (x, der)
extraerMinimo (Nodo x izq der) =
    let (min, nuevaIzq) = extraerMinimo izq
    in (min, Nodo x nuevaIzq der)

-- CONSULTAS Y RECORRIDOS
-- Calcula la altura (profundidad máxima) del árbol
altura:: Arbol a -> Int
altura Hoja = 0
altura (Nodo _ izq der) = 1 + max (altura izq) (altura der)

-- Cuenta la cantidad total de nodos
cantidadNodos :: Arbol a -> Int
cantidadNodos Hoja = 0
cantidadNodos (Nodo _ izq der) = 1 + cantidadNodos izq + cantidadNodos der

-- Busca un elemento en el árbol (eficiencia O(log n) si está balanceado)
buscar:: Ord a => a -> Arbol a -> Bool
buscar x Hoja = False
buscar x (Nodo y izq der)
    | x == y = True
    | x > y  = buscar x der
    | otherwise = buscar x izq

-- Recorrido In-Order (Izquierda -> Raíz -> Derecha) -> Devuelve lista ordenada
inOrder :: Arbol a -> [a]
inOrder Hoja = []
inOrder (Nodo x izq der) = inOrder izq ++ [x] ++ inOrder der

-- Recorrido Pre-Order (Raíz -> Izquierda -> Derecha)
preOrder :: Arbol a -> [a]
preOrder Hoja = []
preOrder (Nodo x izq der) = [x] ++ preOrder izq ++ preOrder der

-- Recorrido Post-Order (Izquierda -> Derecha -> Raíz)
postOrder :: Arbol a -> [a]
postOrder Hoja = []
postOrder (Nodo x izq der) = postOrder izq ++ postOrder der ++ [x]

-- Invierte el árbol (efecto espejo)
espejo :: Arbol a -> Arbol a
espejo Hoja = Hoja
espejo (Nodo x izq der) = Nodo x (espejo der) (espejo izq)