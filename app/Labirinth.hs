module Labirinth (labirinth) where

import Types (Enviroment(Labirinth), Agent(Frontier), baseAlgorithm)

import SearchAlgorithms (
    action,
    findCoordenate,
    perceptionLabirinthBFS,
    perceptionLabirinthDFS,
    perceptionLabirinthGreedy,
    perceptionLabirinthStar)

import Data.List.Split (splitOn)

import Util(tab)

labirinth :: [String] -> IO ()
labirinth [filepath, obstacle, treasure, normal, agent] = do
    matrix <- readLabirinth filepath

    let coordenate = findCoordenate agent matrix "Just one coordenate is possible, but none or more was found."
    let enviroment = Labirinth obstacle treasure normal agent matrix coordenate

    putStrLn $ "Initial coordenate " ++ show coordenate

    let agent = Frontier [] [coordenate] [coordenate] False

    let agents = runner agent enviroment

    printAgents agents

    return ()

labirinth [] = do
    putStrLn "lambda-ai labirinth [file path] [obstacle symbol] [treasure symbol] [normal symbol] [agent symbol]"
    putStrLn ""
    putStrLn $ tab ++ "Parameters:"
    putStrLn $ tab ++ tab ++ "file path: The path for the .txt file that contains the labirinth."
    putStrLn $ tab ++ tab ++ tab ++ "The file need a separator between all characters, the separator need be identification is the '|'."
    putStrLn $ tab ++ tab ++ tab ++ tab ++ "For diferent separator type, use the options"
    putStrLn ""
    putStrLn $ tab ++ tab ++ "obstacle symbol: the symbol used for identify a obstacle."
    putStrLn ""
    putStrLn $ tab ++ tab ++ "treasure symbol: the symbol used for identify the treasure."
    putStrLn ""
    putStrLn $ tab ++ tab ++ "normal symbol: the symbol used for identify a normal spot."
    putStrLn ""
    putStrLn $ tab ++ tab ++ "agent symbol: the symbol used for identify the agent."
    putStrLn ""
    putStrLn $ tab ++ tab ++ "For every symbol, use the apostrofe symbol (') for determinate it."
    putStrLn $ tab ++ tab ++ tab ++ "If the same symbol did use twice, a error gonna be showed."
    putStrLn ""
    putStrLn $ tab ++ "Output"
    putStrLn $ tab ++ tab ++ "A list from agents, describing the way the agent used from the initial to the treasure."
    putStrLn $ tab ++ tab ++ tab ++ "If this doesn't have a way, he just gonna show his last tempt."

labirinth _ = putStrLn "Put the arguments in the right way, you can see it typing \"lambda-ai labirinth\""

printAgents :: [(Agent (Int, Int), String)] -> IO ()
printAgents [] = return ()
printAgents ((Frontier frontier finalState historic _, typeAgent) : agents) = do
    putStrLn $ "Agent " ++ typeAgent
    putStrLn $ "Final way this agent goes " ++ show (reverse finalState)
    putStrLn $ "ways still not explored " ++ show (reverse frontier)
    putStrLn $ "All coordenates this agent reached " ++ show (reverse historic)
    putStrLn "---"
    printAgents agents


runner :: Agent (Int, Int) -> Enviroment String -> [(Agent (Int, Int), String)]
runner agent enviroment = [
    (baseAlgorithm enviroment agent perceptionLabirinthBFS action, "BFS"),
    (baseAlgorithm enviroment agent perceptionLabirinthDFS action, "DFS"),
    (baseAlgorithm enviroment agent perceptionLabirinthStar action, "A*"),
    (baseAlgorithm enviroment agent perceptionLabirinthGreedy action, "Greedy")
    ]

readLabirinth :: String -> IO [[String]]
readLabirinth filepath = do
    txtContent <- readFile filepath
    let linesContent = lines txtContent
    let labirinthMap = map (splitOn "|") linesContent
    return labirinthMap