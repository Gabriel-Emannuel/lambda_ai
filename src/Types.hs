{-# LANGUAGE InstanceSigs #-}
module Types (Agent(..), Enviroment(..), baseAlgorithm, isValidLabirinth) where

data Agent t = Frontier [[t]] [t] [t] | 
               Cromossome [[t]] Int   |
               HillClibing t (t -> Bool) (t -> Bool -> t) |
               SimulatedAnnealing t (t -> Bool) Float Float Float (t -> Float -> t)

data Enviroment t = Labirinth t t t t [[t]] (Int, Int) | 
                    Function  (t -> Float)

instance (Eq t) => Eq (Agent t) where

  (==) :: Eq t => Agent t -> Agent t -> Bool
  (Frontier matrixL stateL historicL) == (Frontier matrixR stateR historicR) = 
    matrixL == matrixR && stateL == stateR && historicL == historicR
  (Cromossome cromossomesL turnsL) == (Cromossome cromossomesR turnsR) = cromossomesL == cromossomesR
  (HillClibing valueL domainL functionModifierL) == (HillClibing valueR domainR functionModifierR) = 
    valueL == valueR
  (SimulatedAnnealing valueL domainL tempL tempRateL tempMinL functionChangeL) == (SimulatedAnnealing valueR domainR tempR tempRateR tempMinR functionChangeR) =
    valueL == valueR
  _ == _ = False
  

baseAlgorithm :: (Eq s) => Enviroment t -> Agent s ->
                 (Enviroment t -> Agent s -> Agent s) ->
                 (Agent s -> Enviroment t -> (Agent s, Enviroment t)) ->
                 Agent s
baseAlgorithm enviroment agent perceptionType action
    | agentAfterPerception == agent = agentAfterPerception
    | otherwise = baseAlgorithm newEnviroment newAgent perceptionType action
    where
        agentAfterPerception = perceptionType enviroment agent
        (newAgent, newEnviroment) = action agent enviroment

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
    && x < length (matrix !! y) 
    && matrix !! y !! x == agent