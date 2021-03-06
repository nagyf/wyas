{-|
Module      : Main
Description : The main module of the application
License     : MIT
Maintainer  : jester.nf@gmail.com
Stability   : stable
Portability : portable

This is the main module of the application, containing the 'main' method.
-}
module Main where

import System.Environment (getArgs)
import REPL

-- |The main method, used to either evaluate an expression or run the REPL.
-- In order to evaluate a lisp expression, you have to pass it as a command line
-- argument. If you want to run the REPL, you have to call main without any arguments.
main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args