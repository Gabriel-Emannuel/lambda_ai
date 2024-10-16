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
action (Frontier frontier state historic end) (Labirinth obstacle treasure normal agent matrix lastAgentCoordenate) = 
    (Frontier newFrontier newState newHistoric end, Labirinth obstacle treasure normal agent newMatrix newAgentCoordenate)
    where
        (newState : newFrontier) = frontier
        newAgentCoordenate = head newState
        newHistoric = newAgentCoordenate : historic
        newMatrix = if matrix !! snd newAgentCoordenate !! fst newAgentCoordenate /= treasure
                    then (insertMatrix agent newAgentCoordenate . insertMatrix normal lastAgentCoordenate) matrix
                    else matrix

---

perceptionLabirinthBase :: (Eq t) => Enviroment t -> Agent (Int, Int) -> Agent (Int, Int)
perceptionLabirinthBase (Labirinth obstacle treasure normal agent matrix agentCoordenate) (Frontier frontier state historic _) =
    Frontier newFrontier state historic newEnd
    where
        perceptions = (map (: state) .
                      filter (`notElem` historic) .
                      filter (isValidPerception (Labirinth obstacle treasure normal agent matrix agentCoordenate)) .
                      generateAllPerceptions) agentCoordenate

        newFrontier = perceptions ++ frontier

        isTreasureReached = findCoordenateTreasure (Labirinth obstacle treasure normal agent matrix agentCoordenate) `elem` historic

        newEnd = isTreasureReached || null newFrontier

perceptionLabirinthDFS :: (Eq t) => Enviroment t -> Agent (Int, Int) -> Agent (Int, Int)
perceptionLabirinthDFS = perceptionLabirinthBase

perceptionLabirinthBFS :: (Eq t) => Enviroment t -> Agent (Int, Int) -> Agent (Int, Int)
perceptionLabirinthBFS labirinth agent
    | end = Frontier frontier state historic end
    | otherwise = Frontier (last frontier : init frontier) state historic end
    where
        Frontier frontier state historic end = perceptionLabirinthBase labirinth agent

perceptionLabirinthStar :: (Ord t, Eq t) => Enviroment t -> Agent (Int, Int) -> Agent (Int, Int)
perceptionLabirinthStar labirinth agent = Frontier newFrontier state historic end
    where
        Frontier frontier state historic end = perceptionLabirinthBase labirinth agent
        sortFunction :: [[(Int, Int)]] -> [[(Int, Int)]]
        sortFunction = sortOn (\y -> manhantanDistance labirinth (head y) + length y)
        newFrontier = (reverse . sortFunction) frontier

perceptionLabirinthGreedy :: (Ord t, Eq t) => Enviroment t -> Agent (Int, Int) -> Agent (Int, Int)
perceptionLabirinthGreedy labirinth agent = Frontier newFrontier state historic end
    where
        Frontier frontier state historic end = perceptionLabirinthBase labirinth agent
        sortFunction :: [[(Int,Int)]] -> [[(Int, Int)]]
        sortFunction = sortOn (manhantanDistance labirinth . head)
        newFrontier = (reverse . sortFunction) frontier
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
isValidPerception (Labirinth _ treasure normal _ matrix _) (x, y) =
    y < length matrix && x < length (matrix !! y) && cursor `elem` [treasure, normal]
    where
        cursor = matrix !! y !! x

---

insertMatrix :: t -> (Int, Int) -> [[t]] -> [[t]]
insertMatrix element (x, y) matrix = take y matrix ++ [take x line ++ [element] ++ drop (succ x) line]++ drop (succ y) matrix
    where
        line = matrix !! y