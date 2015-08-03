module Main where

import System.Environment (getArgs)
import Parser
import Evaluator
import Errors
import Control.Monad


main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled