module Main where

import System.Environment (getArgs)

import Labirinth(labirinth)
import Polynomial(polynomial)
import Genetic(genetic)

import Util(tab)

main :: IO ()
main = do
    args <- getArgs
    selectionType args
    return ()

selectionType :: [String] -> IO ()
selectionType ["lambda-ai"] = showCommands
selectionType ("lambda-ai":"labirinth":arguments) = labirinth arguments
selectionType ("lambda-ai":"polynomial": arguments) = polynomial arguments
selectionType ("lambda-ai":"genetic": arguments) = genetic arguments
selectionType _ = putStrLn "Type \"lambda-ai\" for look the possible commands"

showCommands :: IO ()
showCommands = do
    putStrLn "lambda-ai [problem]"
    putStrLn "problem:"
    putStrLn $ tab ++ "labirinth - Solve a Labirinth already saved in your computer and look how multiple strategies works in the process!"
    putStrLn ""
    putStrLn $ tab ++ "polinomial - From a polinomial and a Value Interval, you can see how multiple strategies find the maximum elements."
    putStrLn ""
    putStrLn $ tab ++ "genetic - From a series of experiments, you can look how genetic algorithms works."
    putStrLn ""
    putStrLn $ tab ++ "For more information, type: \"lambda-ai [problem]\""

{--
        function "polinomial" "minimum from domain" "maximum from domain"
        genetic demonstration
--}