module SearchAlgorithms where

import Types(Agent(Frontier), Enviroment(Labirinth))
import Data.List (sortOn)

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

perceptionLabirinthStar :: (Ord t, Eq t) => Enviroment t -> Agent (Int, Int) -> Agent (Int, Int)
perceptionLabirinthStar labirinth agent = Frontier newFrontier state historic
    where
        (Frontier frontier state historic) = perceptionLabirinthBase labirinth agent
        sortFunction x = (manhantanDistance labirinth . head) x + length x 
        newFrontier = sortOn sortFunction frontier

perceptionLabirinthGreedy :: (Ord t, Eq t) => Enviroment t -> Agent (Int, Int) -> Agent (Int, Int)
perceptionLabirinthGreedy labirinth agent = Frontier newFrontier state historic
    where
        (Frontier frontier state historic) = perceptionLabirinthBase labirinth agent
        newFrontier = sortOn (manhantanDistance labirinth . head) frontier

manhantanDistance :: (Eq t) => Enviroment t -> (Int, Int) -> Int
manhantanDistance labirinth (x,y)= abs (x-i) + abs (y - j)
    where
        (i,j) = findCoordenateTreasure labirinth 

findCoordenateTreasure :: (Eq t) => Enviroment t -> (Int, Int)
findCoordenateTreasure (Labirinth _ treasure _ _ matrix _)
    | null treasuresSpots || length treasuresSpots > 1 = error "You have two Treasure Spots" -- can we implement a way for more than one?
    | otherwise = head treasuresSpots
    where
        treasuresSpots = [(x, y) | y <- [0..(pred.length) matrix], 
                                   x <- [0..(pred.length) (matrix !! y)], 
                                   matrix !! y !! x == treasure]
        

generateAllPerceptions :: (Int, Int) -> [(Int, Int)]
generateAllPerceptions (x, y) = [(succ x, y), (pred x, y), (x, succ y), (x, pred y)]

isValidPerception :: (Eq t) => Enviroment t -> (Int, Int) -> Bool
isValidPerception _ (-1, _) = False
isValidPerception _ (_, -1) = False
isValidPerception (Labirinth obstacle treasure normal agent matrix _) (x, y) =
    y < length matrix && x < length (matrix !! y) && cursor == normal
    where
        cursor = matrix !! y !! x