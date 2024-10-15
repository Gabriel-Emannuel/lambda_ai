module SearchAlgorithms (
    action, 
    perceptionLabirinthBFS, 
    perceptionLabirinthDFS, 
    perceptionLabirinthGreedy, 
    perceptionLabirinthStar,
    findCoordenate) where

import Types(Agent(Frontier), Enviroment(Labirinth))
import Data.List (sortOn)

action :: (Eq t, Show t) => Agent (Int, Int) -> Enviroment t -> (Agent (Int, Int), Enviroment t)
action (Frontier frontier state historic) (Labirinth obstacle treasure normal agent matrix (x, y)) =
    (Frontier newFrontier newState ((x,y):historic), Labirinth obstacle treasure normal agent newMatrix newAgentCoordenate)
    where
        newAgentCoordenate = (head . head) frontier
        newMatrix = (insertMatrix agent newAgentCoordenate . insertMatrix normal (x,y)) matrix
        (newState : newFrontier) = frontier
        newHistoric = (x,y):historic

---

perceptionLabirinthBase :: (Eq t) => Enviroment t -> Agent (Int, Int) -> Agent (Int, Int)
perceptionLabirinthBase (Labirinth obstacle treasure normal agent matrix (x, y)) (Frontier frontier state historic)
    | isAlreadySolved = Frontier frontier state historic
    | otherwise = Frontier (frontier ++ newStates) state historic
    where
        isAlreadySolved = (not . null) [matrix !! y !! x | (x,y) <- historic, matrix !! y !! x == treasure]
        newPerceptions = (filter (\x -> x `notElem` historic) . filter (isValidPerception (Labirinth obstacle treasure normal agent matrix (x, y))) . generateAllPerceptions ) (x, y)
        newStates = if null frontier then map (:[]) newPerceptions else map (: head frontier) newPerceptions

perceptionLabirinthDFS :: (Eq t) => Enviroment t -> Agent (Int, Int) -> Agent (Int, Int)
perceptionLabirinthDFS labirinth agent
    | (perceptionLabirinthBase labirinth agent) == agent = agent
    | otherwise = Frontier (last frontier : init frontier) state historic
    where
        (Frontier frontier state historic) = perceptionLabirinthBase labirinth agent

perceptionLabirinthBFS :: (Eq t) => Enviroment t -> Agent (Int, Int) -> Agent (Int, Int)
perceptionLabirinthBFS = perceptionLabirinthBase

perceptionLabirinthStar :: (Ord t, Eq t) => Enviroment t -> Agent (Int, Int) -> Agent (Int, Int)
perceptionLabirinthStar labirinth agent
    | (perceptionLabirinthBase labirinth agent) == agent = agent
    | otherwise = Frontier newFrontier state historic
    where
        (Frontier frontier state historic) = perceptionLabirinthBase labirinth agent
        sortFunction x = (manhantanDistance labirinth . head) x + length x 
        newFrontier = reverse $ sortOn sortFunction frontier

perceptionLabirinthGreedy :: (Ord t, Eq t) => Enviroment t -> Agent (Int, Int) -> Agent (Int, Int)
perceptionLabirinthGreedy labirinth agent
    | (perceptionLabirinthBase labirinth agent) == agent = agent
    | otherwise = Frontier newFrontier state historic
    where
        (Frontier frontier state historic) = perceptionLabirinthBase labirinth agent
        newFrontier = reverse $ sortOn (manhantanDistance labirinth . head) frontier

---

manhantanDistance :: (Eq t) => Enviroment t -> (Int, Int) -> Int
manhantanDistance labirinth (x,y)= abs (x-i) + abs (y - j)
    where
        (i,j) = findCoordenateTreasure labirinth 

findCoordenateTreasure :: (Eq t) => Enviroment t -> (Int, Int)
findCoordenateTreasure (Labirinth _ treasure _ _ matrix _) = 
    findCoordenate treasure matrix "You have two Treasure Spots"

findCoordenate :: (Eq t) => t -> [[t]] -> String -> (Int, Int)
findCoordenate char matrix message 
    | null spots || length spots > 1 = error message
    | otherwise = head spots
    where 
        spots =  [(x, y) | y <- [0..(pred.length) matrix], 
                  x <- [0..(pred.length) (matrix !! y)], 
                  matrix !! y !! x == char]

---

generateAllPerceptions :: (Int, Int) -> [(Int, Int)]
generateAllPerceptions (x, y) = [(succ x, y), (pred x, y), (x, succ y), (x, pred y)]

isValidPerception :: (Eq t) => Enviroment t -> (Int, Int) -> Bool
isValidPerception _ (-1, _) = False
isValidPerception _ (_, -1) = False
isValidPerception (Labirinth obstacle treasure normal agent matrix _) (x, y) =
    y < length matrix && x < length (matrix !! y) && cursor == normal
    where
        cursor = matrix !! y !! x

---

insertMatrix :: t -> (Int, Int) -> [[t]] -> [[t]]
insertMatrix element (x, y) matrix = take y matrix ++ [take x line ++ [element] ++ drop (succ x) line]++ drop (succ y) matrix
    where
        line = matrix !! y