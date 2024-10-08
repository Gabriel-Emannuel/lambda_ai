module BaseConcepts.Labirinth where

import BaseConcepts.Types (Agent(..))

data Labirinth t = Labirinth t t t t [[t]] (Int, Int)

action :: Agent t -> Labirinth t -> (Agent t, Labirinth t)
action (Reactive []) _ = error "Doesn't exist perceptions on the agent"
action (WithPast [] _) _ = error "Doesn't exist perceptions on the agent"
action (Reactive coordenates) labirinth = (Reactive (tail coordenates), putAgent labirinth (head coordenates))
action (WithPast coordenates history) labirinth = (WithPast (tail coordenates) history, putAgent labirinth (head coordenates))

putAgent :: Labirinth t -> (Int, Int) -> Labirinth t
putAgent (Labirinth obstacle treasure agentCursor normalValue matrix (x,y)) (i,j) = 
    Labirinth obstacle treasure agentCursor normalValue ((putCursor normalValue (x,y) . putCursor agentCursor (i,j)) matrix) 

putCursor :: t -> (Int, Int) -> [[t]] -> [[t]]
putCursor agentCursor (x,y) matrix = 
    take y matrix ++ [take x line ++ [agentCursor] ++ drop (succ x) line] ++ drop (succ y) matrix
    where
        line = matrix !! y

perceptionLabirinth :: (Eq t) => Labirinth t -> Agent [(Int,Int)] -> Agent [(Int, Int)]
perceptionLabirinth labirinth (Reactive coordenates) 
    | null coordenateWinning = Reactive (coordenateObstacle ++ coordenates)
    | otherwise = Reactive coordenateWinning
    where
        coordenateObstacle = (filterNotObstacle labirinth . getAllCoordenates) labirinth
        coordenateWinning = filterJustFinalSpots labirinth coordenateObstacle

perceptionLabirinth labirinth (WithPast coordenates history) = 
    | null coordenateWinning = WithPast (coordenateObstacle ++ coordenates) (coordenates : history)
    | otherwise = WithPast (coordenateWinning) (coordenates : history)
    where
        coordenateObstacle = (filterNotObstacle labirinth . getAllCoordenates) labirinth
        coordenateWinning = filterJustFinalSpots labirinth coordenateObstacle

getAllCoordenates :: Labirinth t -> [(Int, Int)]
getAllCoordenates (Labirinth _ _ _ _ _ (x, y)) = [(succ x, y), (pred x, y), (x, succ y), (x, pred y)]

filterValid :: (Int, Int) -> Int -> Bool
filterValid (-1, _) _ = False
filterValid (_, -1) _ = False
filterValid (x, y)  l = x < l && y < l

filterNotObstacle :: (Eq t) -> Labirinth t -> [(Int, Int)] -> [(Int, Int)]
filterNotObstacle (Labirinth a _ _ _ matrix _) coordenates = 
    (filter (\(i,j) -> matrix !! j !! i == a) . filter filterValid ) coordenates

filterJustFinalSpots :: (Eq t) -> Labirinth t -> [(Int, Int)] -> [(Int, Int)]
filterJustFinalSpots (Labirinth _ a _ _ matrix _) coordenates = filter (\(i, j) -> matrix !! j !! i == a) coordenates