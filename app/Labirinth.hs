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
labirinth (filepath : obstacle : treasure : normal : agent : options) = do
    matrix <- readLabirinth filepath options


    let coordenate = findCoordenate agent matrix "Just one coordenate is possible, but none or more was found."
    let enviroment = Labirinth obstacle treasure normal agent matrix coordenate

    putStrLn $ "Initial coordenate " ++ show coordenate

    let agent = Frontier [[coordenate]] [] [] False

    let agents = runner options agent enviroment

    printAgents agents

    return ()

labirinth [] = do
    putStrLn "lambda-ai labirinth [file path] [obstacle symbol] [treasure symbol] [normal symbol] [agent symbol] -[option]"
    putStrLn ""
    putStrLn $ tab ++ "Parameters:"
    putStrLn $ tab ++ tab ++ "file path: The path for the .txt file that contains the labirinth."
    putStrLn $ tab ++ tab ++ tab ++ "The file need a separator between all characters, the default separator for identification is the '|'."
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
    putStrLn $ tab ++ tab ++ "Option"
    putStrLn $ tab ++ tab ++ tab ++ "Separator [separator]"
    putStrLn $ tab ++ tab ++ tab ++ tab ++ "The separator used between the characters"
    putStrLn $ tab ++ tab ++ tab ++ "Algorithm [algorithm]"
    putStrLn $ tab ++ tab ++ tab ++ tab ++ "The algorithm used can be:"
    putStrLn $ tab ++ tab ++ tab ++ tab ++ tab ++ "A*"
    putStrLn $ tab ++ tab ++ tab ++ tab ++ tab ++ "BFS"
    putStrLn $ tab ++ tab ++ tab ++ tab ++ tab ++ "DFS"
    putStrLn $ tab ++ tab ++ tab ++ tab ++ tab ++ "Greedy"    
    putStrLn ""
    putStrLn $ tab ++ tab ++ "Output"
    putStrLn $ tab ++ tab ++ tab ++ "A list from agents, describing the way the agent used from the initial to the treasure."
    putStrLn $ tab ++ tab ++ tab ++ tab ++ "If this doesn't have a way, he just gonna show his last tempt."

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


runner :: [String] -> Agent (Int, Int) -> Enviroment String -> [(Agent (Int, Int), String)]
runner ("Separator" : _ : options) agent enviroment = runner options agent enviroment
runner ("Algorithm" : algorithm : _) agent enviroment
    | algorithm == "A*" = [(baseAlgorithm enviroment agent perceptionLabirinthStar action, "A*")]
    | algorithm == "BFS" = [(baseAlgorithm enviroment agent perceptionLabirinthBFS action, "BFS")]
    | algorithm == "DFS" = [(baseAlgorithm enviroment agent perceptionLabirinthDFS action, "DFS")]
    | algorithm == "Greedy" = [(baseAlgorithm enviroment agent perceptionLabirinthGreedy action, "Greedy")]
    | otherwise = error "Algorithm doesn't registred for be used."
runner _ agent enviroment = [
    (baseAlgorithm enviroment agent perceptionLabirinthBFS action, "BFS"),
    (baseAlgorithm enviroment agent perceptionLabirinthDFS action, "DFS"),
    (baseAlgorithm enviroment agent perceptionLabirinthStar action, "A*"),
    (baseAlgorithm enviroment agent perceptionLabirinthGreedy action, "Greedy")
    ]

readLabirinth :: String -> [String] -> IO [[String]]
readLabirinth filepath ("Algorithm" : _ : options) = readLabirinth filepath options
readLabirinth filepath ("Separator" : separator : _) = do
    txtContent <- readFile filepath
    let linesContent = lines txtContent
    let labirinthMap = map (splitOn separator) linesContent
    return labirinthMap

readLabirinth filepath _ = readLabirinth filepath ["Separator", "|"]