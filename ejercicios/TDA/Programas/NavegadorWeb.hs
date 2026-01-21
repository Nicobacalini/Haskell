module NavegadorWeb where

newtype Stack a = Stk [a] deriving Show

push:: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)

pop :: Stack a -> Stack a
pop (Stk (x:xs)) = Stk xs

top :: Stack a -> a
top (Stk (x:xs)) = x

type Historial = Stack String

-- Agrega una nueva página al historial
visitarPagina :: String -> Historial -> Historial
visitarPagina url historial = push url historial

-- Vuelve a la página anterior en el historial
volverAtras :: Historial -> (String, Historial)
volverAtras historial = (top historial, pop historial)

-- Obtiene la URL de la ultima pagina visitada
ultimaPagina :: Historial -> String
ultimaPagina historial = top historial

-- Muestra el historial completo en formato de lista legible
mostrarHistorial :: Historial -> [String]
mostrarHistorial (Stk lista) = lista

-- Verifica si hay páginas anteriores para habilitar el botón "Atrás"
puedoVolver :: Historial -> Bool
puedoVolver (Stk []) = False
puedoVolver (Stk _)  = True

-- Vaciar historial (como "Borrar datos de navegación")
borrarHistorial :: Historial
borrarHistorial = Stk []