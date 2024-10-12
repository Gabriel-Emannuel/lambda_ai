module LocalSearch () where

import Types (Agent(HillClibing, SimulatedAnnealing), Enviroment(Function), baseAlgorithm)
import System.Random (randomRIO)

---

hillClibingPerception :: (Ord t) => Enviroment t -> Agent t -> Agent t
hillClibingPerception (Function  function) (HillClibing value domain functionChange) 
    | valueL `elem` domain && function valueL > function value = HillClibing valueL domain functionChange
    | valueR `elem` domain && function valueR > function value = HillClibing valueR domain functionChange
    | otherwise = HillClibing value domain functionChange
    where
        valueL = functionChange value False
        valueR = functionChange value True

hillClibingAction :: Agent s -> Enviroment t -> (Agent s, Enviroment t)
hillClibingAction (HillClibing value domain functionChange) (Function  function) = (HillClibing value domain functionChange, Function  function)

hillClibing :: (Eq t, Ord t) => (t -> Int) -> (t -> Bool -> t) -> [t] -> IO (Agent t)
hillClibing function functionChange domain = do
    indexElement <- randomRIO (0, length domain)
    let agent = HillClibing (domain !! indexElement) domain functionChange
    return $ baseAlgorithm enviroment agent hillClibingPerception hillClibingAction
    where
        enviroment = Function function

---

simulatedAnnealingPerception :: (Ord t) => Enviroment t -> Agent t -> Agent t
simulatedAnnealingPerception (Function function) (SimulatedAnnealing value domain temp tempRate tempMin functionChange)
    | temp <= tempMin = SimulatedAnnealing value domain temp tempRate tempMin functionChange
    | valueL `elem` domain && function valueL > function value = SimulatedAnnealing valueL domain temp tempRate tempMin functionChange
    | valueR `elem` domain && function valueR > function value = SimulatedAnnealing valueR domain temp tempRate tempMin functionChange
    | otherwise = SimulatedAnnealing value domain temp tempRate tempMin functionChange
    where
        valueL = functionChange value temp
        valueR = functionChange value (temp * (-1))

simulatedAnnealingAction :: Agent s -> Enviroment t -> (Agent s, Enviroment t)
simulatedAnnealingAction (SimulatedAnnealing value domain temp tempRate tempMin functionChange) (Function function) = 
    (SimulatedAnnealing value domain (temp * tempRate) tempRate tempMin functionChange ,Function function)

simulatedAnnealing :: (Ord t) => (t -> Int) -> (t -> Float -> t) -> Float -> Float -> Float -> [t] -> IO (Agent t)
simulatedAnnealing function functionChange tempInitial tempRate tempMin domain = do
    indexElement <- randomRIO (0, length domain)
    let agent = SimulatedAnnealing (domain !! indexElement) domain tempInitial tempRate tempMin functionChange
    return $ baseAlgorithm enviroment agent simulatedAnnealingPerception simulatedAnnealingAction
    where
        enviroment = Function function