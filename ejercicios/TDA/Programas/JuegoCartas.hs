module JuegoCartas where

data Palo = Copa | Oro | Basto | Espada deriving (Show, Eq, Ord)
data Carta = C Int Palo deriving Show

instance Eq Carta where
    (==) (C v1 _) (C v2 _) = v1 == v2

-- Definir el orden de las cartas según su valor y palo
instance Ord Carta where
    compare (C v1 p1) (C v2 p2) 
        | v1 > v2   = GT       
        | v1 < v2   = LT       
        | otherwise = compare p1 p2

-- Generar todas las cartas de un palo
cartasDePalo :: Palo -> [Carta]
cartasDePalo p = [C v p | v <- [1..12], v /= 8, v /= 9]

-- Generar el mazo completo
mazoCompleto :: [Carta]
mazoCompleto = cartasDePalo Copa ++ cartasDePalo Oro ++ cartasDePalo Basto ++ cartasDePalo Espada

-- Determinar si la carta 1 le gana a la 2
ganaMano :: Carta -> Carta -> String
ganaMano c1 c2
    | c1 > c2   = "Gana la primera"
    | c2 > c1   = "Gana la segunda"
    | otherwise = "Empate (Parda)"