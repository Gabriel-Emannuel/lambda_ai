module Polynomial (polynomial) where

import Types(Agent(HillClibing, SimulatedAnnealing), Enviroment(Function))
import LocalSearch (hillClibing, simulatedAnnealing)

import Util(tab)

polynomial :: [String] -> IO ()
polynomial (intervalMin : intervalMax : function) = do

    agentHill      <- hillClibing f functionChangeHill domain possibleValues
    agentSimulated <- simulatedAnnealing f functionChangeSimulatedAnnealing (10^^2) 0.1 10 possibleValues domain

    printAgent agentHill
    printAgent agentSimulated

    where
        valueMinimum = convertFloat intervalMin
        valueMaximum = convertFloat intervalMax
        valuesFunction = map convertFloat function
        
        domain :: Float -> Bool
        domain v = v >= min valueMinimum valueMaximum && v <= max valueMinimum valueMaximum

        f :: Float -> Float
        f v = (sum . zipWith (*) [v ^^ exp | exp <- [0..(pred . length) function]]) valuesFunction

        varInterval = (abs (valueMinimum - valueMaximum)) / (max valueMinimum valueMaximum)

        possibleValues = [min valueMinimum valueMaximum .. max valueMinimum valueMaximum]

        functionChangeHill :: Float -> Bool -> Float
        functionChangeHill v True  = v + varInterval
        functionChangeHill v False = v - varInterval

        functionChangeSimulatedAnnealing :: Float -> Float -> Float
        functionChangeSimulatedAnnealing v temp = v + temp

polynomial [] = do
    putStrLn "lambda-ai polynomial [minimum value] [maximum value] [function]"
    putStrLn ""
    putStrLn $ tab ++ "Paramenters:"
    putStrLn $ tab ++ tab ++ "Minimum Value: A float value that represent the minimum value in the function's domain;"
    putStrLn $ tab ++ tab ++ "Maximum Value: A float value that represent the maximum value in the function's domain;"
    putStrLn $ tab ++ tab ++ "function:      A series of floats values, every value is in the same order:"
    putStrLn $ tab ++ tab ++ "               value1 * x^0 + value2 * x^1 + ..."
    putStrLn ""
    putStrLn $ tab ++ "Output:"
    putStrLn $ tab ++ tab ++ "All maximum local values reached by the algorithms and showing all the algorithms."
    
polynomial _ = putStrLn "Put the arguments in the right way, you can see it typing \"lambda-ai polynomial\""

printAgent :: (Show t) => Agent t -> IO ()
printAgent (HillClibing maxValue _ _ _) = do
    putStrLn "---"
    putStrLn   "Hill Clibing"
    putStrLn $ "The maximum local reached is: " ++ show maxValue 
    putStrLn "---"
printAgent (SimulatedAnnealing maxValue _ _ _ _ _ _) = do
    putStrLn "---"
    putStrLn    "Simultaded Annealing"
    putStrLn $ "The maximum local reached is: " ++ show maxValue 
    putStrLn "---"

convertFloat :: String -> Float
convertFloat v 
    | head v == 'n' = (-1) * (convertFloat . tail) v
    | otherwise = read v :: Float