module ModelosAplicados where

-- Ejercicio 1: DICCIONARIO (Clave -> Valor)
data Arbol a b = EmptyBT | Nodo a b (Arbol a b) (Arbol a b) deriving (Show)
-- a: Clave (ej. Código Producto, Hora Turno) -> Debe ser Ord
-- b: Valor (ej. Nombre Producto, Nombre Paciente)

-- 1. Insertar / Agendar / Agregar
insertar :: Ord a => a -> b -> Arbol a b -> Arbol a b
insertar k v EmptyBT = Nodo k v EmptyBT EmptyBT
insertar k v (Nodo clave valor izq der)
    | k == clave = Nodo clave v izq der         -- Actualizo valor si ya existe
    | k < clave  = Nodo clave valor (insertar k v izq) der
    | otherwise  = Nodo clave valor izq (insertar k v der)

-- 2. Buscar / Consultar
buscar :: Ord a => a -> Arbol a b -> b
buscar _ EmptyBT = error "Clave no encontrada"
buscar k (Nodo clave valor izq der)
    | k == clave = valor
    | k < clave  = buscar k izq
    | otherwise  = buscar k der

-- Ejercicio 2: TURNERO MÉDICO (Basado en Arbol)
-- Usamos 'a' como Hora (Int) y 'b' como Paciente (String)
newtype Turnero a b = T (Arbol a b) deriving Show

nuevoTurnero :: Turnero a b
nuevoTurnero = T EmptyBT

agendarTurno :: Ord a => a -> b -> Turnero a b -> Turnero a b
agendarTurno hora paciente (T arbol) = T (insertar hora paciente arbol)

buscarPaciente :: Ord a => a -> Turnero a b -> b
buscarPaciente hora (T arbol) = buscar hora arbol

-- Cancelar Turno (Borrado en BST con Clave-Valor)
cancelarTurno :: Ord a => a -> Turnero a b -> Turnero a b
cancelarTurno hora (T arbol) = T (borrar hora arbol)

-- Auxiliares de borrado
borrar :: Ord a => a -> Arbol a b -> Arbol a b
borrar _ EmptyBT = EmptyBT
borrar k (Nodo clave valor izq der)
    | k < clave = Nodo clave valor (borrar k izq) der
    | k > clave = Nodo clave valor izq (borrar k der)
    | otherwise = unirArboles izq der -- Encontramos el nodo

unirArboles :: Arbol a b -> Arbol a b -> Arbol a b
unirArboles EmptyBT der = der
unirArboles izq EmptyBT = izq
unirArboles izq der = 
    let (minK, minV) = minimo der
        nuevaDer     = borrar minK der
    in Nodo minK minV izq nuevaDer

minimo :: Arbol a b -> (a, b)
minimo (Nodo k v EmptyBT _) = (k, v)
minimo (Nodo _ _ izq _)     = minimo izq
minimo EmptyBT              = error "Arbol vacio"

-- Ejercicio 3: INVENTARIO
-- Listar productos ordenados por código (InOrder)
data Inventario = InvVacio | Item Int String Inventario Inventario deriving Show

listarInventario :: Inventario -> [String]
listarInventario InvVacio = []
listarInventario (Item _ nombre izq der) = 
    listarInventario izq ++ [nombre] ++ listarInventario der