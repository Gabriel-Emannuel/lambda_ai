module Main where

import Test.HUnit (runTestTT, Test(TestList))

main :: IO()

main = do
    runTestTT (TestList [])
    return ()