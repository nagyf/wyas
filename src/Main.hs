module Main where

import System.Environment (getArgs)
import Parser
import Evaluator

main :: IO ()
main = getArgs >>= print . eval . readExpr . head