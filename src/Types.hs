module Types (Agent(..), Enviroment(..), isValidLabirinth) where

data Agent t = Frontier [[t]] [t] [t] deriving (Show, Eq)

data Enviroment t = Labirinth t t t t [[t]] (Int, Int)

isValidLabirinth :: (Eq t) => Enviroment t -> Bool
isValidLabirinth (Labirinth obstacle treasure normal agent matrix coordenates) = 
    isJustValidCursors && isValidCoordenates matrix agent coordenates
    where 
        validCursors = [obstacle, treasure, normal, agent]
        isJustValidCursors = null [element | line <- matrix, 
                                             element <- line, 
                                             element `notElem` validCursors]

isValidCoordenates :: (Eq t) => [[t]] -> t -> (Int, Int) -> Bool
isValidCoordenates _ _ (-1, _) = False
isValidCoordenates _ _ (_, -1) = False
isValidCoordenates matrix agent (x, y) = 
    y < length matrix 
    && x < (length . head) matrix 
    && matrix !! y !! x == agent