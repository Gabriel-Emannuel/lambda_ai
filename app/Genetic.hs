module Genetic (genetic) where

import Util(tab)

genetic :: [String] -> IO ()
genetic [] = do
    putStrLn "lambda-ai genetic"
    putStrLn "The CLI interface for consume the Genetic Algorithm isn't implemented."

genetic _ = putStrLn "Put the arguments in the right way, you can see it typing \"lambda-ai polynomial\""