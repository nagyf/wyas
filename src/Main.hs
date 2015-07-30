module Main where

import System.Environment (getArgs)
import Parser

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ readExpr $ args !! 0