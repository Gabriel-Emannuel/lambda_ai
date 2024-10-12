module GeneticAlgorithms (baseGeneticAlgorithm) where

import Types (Agent(Cromossome))

import System.Random
import Control.Monad (replicateM)
import Data.List (nub)

data Paramenters t = Configuration Int Int Int Int

isValidParamenters :: Paramenters t-> Bool
isValidParamenters (Configuration selectionRatio mutationRatio reproductionRatio population)  
    | selectionRatio < 0 || mutationRatio < 0 || reproductionRatio < 0 || population < 0 = False
    | selectionRatio > population || mutationRatio + reproductionRatio > population = False  
    | otherwise = True

baseGeneticAlgorithm ::  Agent t -> Paramenters t -> ([t] -> Int) -> (Agent t -> Bool) -> IO (Agent t)
baseGeneticAlgorithm (Cromossome cromossomes turns) (Configuration selectionRatio mutationRatio reproductionRatio population) fitnessFunction stopFunction 
    | stopFunction (Cromossome cromossomes turns) = return $ Cromossome cromossomes turns
    | otherwise =  
        do
            roulete <- shuffle [cromossome | cromossome <- cromossomes, _ <- [0..fitnessFunction cromossome]]

            let selected = take selectionRatio roulete

            allPairs <- shuffle [(dad, mom) | dad <- selected, mom <- selected]

            allReproductions <- mapM reproduction allPairs

            allNewByReproduction <- shuffle $ concatMap (\(f,s) -> [f, s]) allReproductions

            allMutations <- shuffle selected

            finalMutation <- shuffle $ take mutationRatio allMutations ++ 
                                       take reproductionRatio allNewByReproduction ++ 
                                       take (population - (mutationRatio + reproductionRatio)) roulete

            baseGeneticAlgorithm (Cromossome finalMutation (succ turns)) (Configuration selectionRatio mutationRatio reproductionRatio population) fitnessFunction stopFunction 

---

shuffle :: [t] -> IO [t]
shuffle array = do
    indexesForShuffle <- replicateM (length array) (randomRIO (0, length array) :: IO Int)
    return $ shuffleAux indexesForShuffle array 

shuffleAux :: [Int] -> [t] -> [t]
shuffleAux [] array = array
shuffleAux (i:finalArray) array = shuffleAux finalArray (right ++ left)
    where
        (left, right) = splitAt i array

---

reproduction :: ([t], [t]) -> IO ([t], [t])
reproduction (cromossomeL, cromossomeR) = do
    pointAmmount <- randomRIO (1, length cromossomeL) :: IO Int
    indexes <- replicateM pointAmmount (randomRIO (0, length cromossomeL) :: IO Int)
    return $ reproductionAux (nub indexes) (cromossomeL, cromossomeR)

reproductionAux :: [Int] -> ([t], [t]) -> ([t], [t])
reproductionAux [] (cromossomeL, cromossomeR) = (cromossomeL, cromossomeR) 
reproductionAux (i:is) (cromossomeL, cromossomeR) = 
    reproductionAux is (leftCromossomeL ++ rightCromossomeR, leftCromossomeR ++ rightCromossomeL)
    where
        (leftCromossomeL, rightCromossomeL) = splitAt i cromossomeL
        (leftCromossomeR, rightCromossomeR) = splitAt i cromossomeR

---

mutation :: [t] -> IO [t]
mutation cromossome = do
    i <- randomRIO (0, length cromossome)
    j <- randomRIO (0, length cromossome)
    return $ mutationAux cromossome (i,j)

mutationAux :: [t] -> (Int, Int) -> [t]
mutationAux cromossome (i,j) = 
    take minimumCoordenate cromossome ++ [cromossome !! minimumCoordenate] ++ 
    (drop (succ minimumCoordenate) . take maximumCoordenate) cromossome  ++ 
    [cromossome !! maximumCoordenate] ++ drop (succ maximumCoordenate) cromossome
    where 
        minimumCoordenate = min i j
        maximumCoordenate = max i j