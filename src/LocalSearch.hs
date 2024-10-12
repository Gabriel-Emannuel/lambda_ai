module LocalSearch (hillClibing, simulatedAnnealing) where

import Types (Agent(HillClibing, SimulatedAnnealing), Enviroment(Function), baseAlgorithm)
import System.Random (randomRIO)
import Data.Foldable (maximumBy)
import Data.Function (on)

---

hillClibing :: (Eq t, Ord t) => (t -> Float) -> (t -> Bool -> t) -> (t -> Bool) -> [t] -> IO (Agent t)
hillClibing function functionChange domain possibleValues = do
    indexElement <- randomRIO (0, (pred .length) possibleValues)
    let agent = HillClibing (possibleValues !! indexElement) domain functionChange
    return $ baseAlgorithm enviroment agent hillClibingPerception hillClibingAction
    where
        enviroment = Function function

simulatedAnnealing :: (Ord t) => (t -> Float) -> (t -> Float -> t) -> Float -> Float -> Float -> [t] -> (t -> Bool) -> IO (Agent t)
simulatedAnnealing function functionChange tempInitial tempRate tempMin possibleValues domain = do
    indexElement <- randomRIO (0, (pred .length) possibleValues)
    let agent = SimulatedAnnealing (possibleValues !! indexElement) domain tempInitial tempRate tempMin functionChange
    return $ baseAlgorithm enviroment agent simulatedAnnealingPerception simulatedAnnealingAction
    where
        enviroment = Function function

---

hillClibingPerception :: (Ord t) => Enviroment t -> Agent t -> Agent t
hillClibingPerception (Function  function) (HillClibing value domain functionChange)
    | domain valueL && function valueL > function value = HillClibing valueL domain functionChange
    | domain valueL && function valueR > function value = HillClibing valueR domain functionChange
    | otherwise = HillClibing value domain functionChange
    where
        valueL = functionChange value False
        valueR = functionChange value True

hillClibingAction :: Agent s -> Enviroment t -> (Agent s, Enviroment t)
hillClibingAction (HillClibing value domain functionChange) (Function  function) = (HillClibing value domain functionChange, Function  function)

---

simulatedAnnealingPerception :: (Ord t) => Enviroment t -> Agent t -> Agent t
simulatedAnnealingPerception (Function function) (SimulatedAnnealing value domain temp tempRate tempMin functionChange)
    | temp <= tempMin = SimulatedAnnealing value domain temp tempRate tempMin functionChange
    | domain valueL && function valueL > function value = SimulatedAnnealing valueL domain temp tempRate tempMin functionChange
    | domain valueR && function valueR > function value = SimulatedAnnealing valueR domain temp tempRate tempMin functionChange
    | otherwise = SimulatedAnnealing value domain temp tempRate tempMin functionChange
    where
        valueL = functionChange value temp
        valueR = functionChange value (temp * (-1))

simulatedAnnealingAction :: Agent s -> Enviroment t -> (Agent s, Enviroment t)
simulatedAnnealingAction (SimulatedAnnealing value domain temp tempRate tempMin functionChange) (Function function) =
    (SimulatedAnnealing value domain (temp * tempRate) tempRate tempMin functionChange ,Function function)

---