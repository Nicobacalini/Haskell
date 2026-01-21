module SistemaLogistica where

data Estado = Pendiente | EnCamino | Entregado deriving Show
data Paquete = Crear {idPkg:: Integer, destino:: String, peso:: Float, status:: Estado} deriving Show
data Bintree a = EmptyBT | NodoBT a (Bintree a) (Bintree a) deriving Show

type Ciudad = String
type Distancia = Float  
type Grafo = [(Ciudad, [(Ciudad, Distancia)])]

instance Eq Paquete where
    (==) p1 p2 = idPkg p1 == idPkg p2

instance Ord Paquete where
    (<=) p1 p2 = idPkg p1 <= idPkg p2

newtype Queue a = Q [a] deriving Show

enQueue :: Paquete -> Queue Paquete -> Queue Paquete
enQueue x (Q xs) = Q (xs ++ [x])

-- Convierte una lista cruda de paquetes en una cola de prioridad
llegadaCamion :: [Paquete] -> Queue Paquete
llegadaCamion lista = Q lista

-- Verifica si el árbol no contiene ningún dato
esVacio :: Bintree a -> Bool
esVacio EmptyBT = True
esVacio _       = False

-- Almacena un paquete en el árbol binario
almacenar :: Paquete -> Bintree Paquete -> Bintree Paquete
almacenar pNuevo EmptyBT = NodoBT pNuevo EmptyBT EmptyBT
almacenar pNuevo (NodoBT pActual izq der)
    | pNuevo == pActual = NodoBT pActual izq der
    | pNuevo < pActual  = NodoBT pActual (almacenar pNuevo izq) der   
    | pNuevo > pActual  = NodoBT pActual izq (almacenar pNuevo der)            
    
-- Busca un paquete por su ID en el árbol binario
buscarPaquete :: Integer -> Bintree Paquete -> Bool
buscarPaquete idBuscado EmptyBT = False
buscarPaquete idBuscado (NodoBT pActual izq der)
    | idBuscado == idPkg pActual = True        
    | idBuscado <  idPkg pActual = buscarPaquete idBuscado izq      
    | idBuscado >  idPkg pActual = buscarPaquete idBuscado der 

-- Extrae el paquete con el ID mínimo del árbol y devuelve el paquete junto con el árbol actualizado
extraerMinimo :: Bintree Paquete -> (Paquete, Bintree Paquete)
extraerMinimo (NodoBT p EmptyBT der) = ( p , der )
extraerMinimo (NodoBT p izq der) =
    let (minimo, nuevaIzq) = extraerMinimo izq 
    in (minimo, NodoBT p nuevaIzq der)

-- Elimina un paquete del sistema una vez que ha sido entregado
entregaPaquete :: Paquete -> Bintree Paquete -> Bintree Paquete
entregaPaquete _ EmptyBT = EmptyBT
entregaPaquete pBuscado (NodoBT pActual izq der)
    | pBuscado < pActual = NodoBT pActual (entregaPaquete pBuscado izq) der
    | pBuscado > pActual = NodoBT pActual izq (entregaPaquete pBuscado der)
    | pBuscado == pActual = 
        if (esVacio der) then izq
        else if (esVacio izq) then der
        else 
            let (minimoDer, nuevaDer) = extraerMinimo der
            in NodoBT minimoDer izq nuevaDer

-- Genera un reporte ordenado de los paquetes almacenados
generarReporte :: Bintree Paquete -> [Paquete]
generarReporte EmptyBT = []
generarReporte (NodoBT p izq der) =
    generarReporte izq ++ [p] ++ generarReporte der

-- Verifica si dos ciudades son vecinas en el grafo
sonVecinos :: Ciudad -> Ciudad -> Grafo -> Bool
sonVecinos origen destino mapa =
    case lookup origen mapa of
        Nothing -> False
        Just adyacentes -> any (\(ciudad, _) -> ciudad == destino) adyacentes
