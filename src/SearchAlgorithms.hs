module SearchAlgorithms where

import Types(Agent(Frontier), Enviroment(Labirinth))

baseAlgorithm :: (Eq s) => Enviroment t -> Agent s ->
                 (Enviroment t -> Agent s -> Agent s) ->
                 (Agent s -> Enviroment t -> (Agent s, Enviroment t)) ->
                 Agent s
baseAlgorithm labirinth agent perceptionType action
    | agentAfterPerception == agent = agentAfterPerception
    | otherwise = baseAlgorithm newEnviroment newAgent perceptionType action
    where
        agentAfterPerception = perceptionType labirinth agent
        (newAgent, newEnviroment) = action agent labirinth

action :: (Eq t) => Agent (Int, Int) -> Enviroment t -> (Agent (Int, Int), Enviroment t)
action (Frontier frontier state historic) (Labirinth obstacle treasure normal agent matrix (x, y)) =
    (Frontier newFrontier newState ((x,y):historic), Labirinth obstacle treasure normal agent newMatrix newAgentCoordenate)
    where
        newAgentCoordenate = (head . head) frontier
        newMatrix = (insertMatrix agent newAgentCoordenate . insertMatrix normal (x,y)) matrix
        (newState : newFrontier) = frontier

insertMatrix :: t -> (Int, Int) -> [[t]] -> [[t]]
insertMatrix element (x, y) matrix = 
    take y matrix ++ [take x line ++ [element] ++ drop (succ x) line]++ drop (succ y) matrix
    where
        line = matrix !! y

perceptionLabirinthBase :: (Eq t) => Enviroment t -> Agent (Int, Int) -> Agent (Int, Int)
perceptionLabirinthBase (Labirinth obstacle treasure normal agent matrix (x, y)) (Frontier frontier state historic)
    | isAlreadySolved = Frontier frontier state historic
    | otherwise = Frontier (frontier ++ newStates) state historic
    where
        isAlreadySolved = (not . null) [matrix !! y !! x | (x,y) <- historic, matrix !! y !! x == treasure]
        newPerceptions = (filter (isValidPerception (Labirinth obstacle treasure normal agent matrix (x, y))) . generateAllPerceptions ) (x, y)
        newStates = map (: head frontier) newPerceptions

perceptionLabirinthDFS :: (Eq t) => Enviroment t -> Agent (Int, Int) -> Agent (Int, Int)
perceptionLabirinthDFS labirinth agent = Frontier (last frontier : init frontier) state historic
    where
        (Frontier frontier state historic) = perceptionLabirinthBase labirinth agent

perceptionLabirinthBFS :: (Eq t) => Enviroment t -> Agent (Int, Int) -> Agent (Int, Int)
perceptionLabirinthBFS = perceptionLabirinthBase

generateAllPerceptions :: (Int, Int) -> [(Int, Int)]
generateAllPerceptions (x, y) = [(succ x, y), (pred x, y), (x, succ y), (x, pred y)]

isValidPerception :: (Eq t) => Enviroment t -> (Int, Int) -> Bool
isValidPerception _ (-1, _) = False
isValidPerception _ (_, -1) = False
isValidPerception (Labirinth obstacle treasure normal agent matrix _) (x, y) =
    y < length matrix && x < (length . head) matrix && cursor == normal
    where
        cursor = matrix !! y !! x