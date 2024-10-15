{-# LANGUAGE InstanceSigs #-}
module Types (Agent(..), Enviroment(..), baseAlgorithm, isValidLabirinth) where

data Agent t = Frontier [[t]] [t] [t] Bool | 
               Cromossome [[t]] Int |
               HillClibing t (t -> Bool) (t -> Bool -> t) Bool |
               SimulatedAnnealing t (t -> Bool) Float Float Float (t -> Float -> t) Bool

data Enviroment t = Labirinth t t t t [[t]] (Int, Int) | 
                    Function  (t -> Float)

instance (Eq t) => Eq (Agent t) where

  (==) :: Eq t => Agent t -> Agent t -> Bool
  (Frontier frontierL stateL historicL _) == (Frontier frontierR stateR historicR _) = 
    frontierL == frontierR && stateL == stateR && historicL == historicR
  (Cromossome cromossomesL _) == (Cromossome cromossomesR _) = cromossomesL == cromossomesR
  (HillClibing valueL _ _ _) == (HillClibing valueR _ _ _) = 
    valueL == valueR
  (SimulatedAnnealing valueL _ _ _ _ _ _) == (SimulatedAnnealing valueR _ _ _ _ _ _) =
    valueL == valueR
  _ == _ = False   

endAlgorithm :: Agent t -> Bool
endAlgorithm (Frontier _ _ _ end) = end
endAlgorithm (HillClibing _ _ _ end) = end
endAlgorithm (SimulatedAnnealing _ _ _ _ _ _ end) = end

baseAlgorithm :: (Eq s) => Enviroment t -> Agent s ->
                 (Enviroment t -> Agent s -> Agent s) ->
                 (Agent s -> Enviroment t -> (Agent s, Enviroment t)) ->
                 Agent s
baseAlgorithm enviroment agent perceptionType action
    | endAlgorithm agentAfterPerception = agentAfterPerception
    | otherwise = baseAlgorithm newEnviroment newAgent perceptionType action
    where
        agentAfterPerception = perceptionType enviroment agent
        (newAgent, newEnviroment) = action agentAfterPerception enviroment

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