{-|
Module      : REPL
Description : Read-eval-print-loop
License     : MIT
Maintainer  : jester.nf@gmail.com
Stability   : stable
Portability : portable

A module that implements a very basic REPL (read-eval-print-loop) like the
haskell REPL.
-}
module REPL(runRepl, evalAndPrint) where

import System.IO
import Control.Monad
import Evaluator
import Parser
import Errors

-- |Print the prompt without a newline character, and flush the buffer
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- |Print the prompt, and read until newline character
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- |Reads the expression, and evaluates it as a lisp expression
evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

-- |Evaluates the expression using 'evalString', and prints the results
evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

-- |Repeats a monadic action, until the given boolean expression evaluates to false
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action

-- |Run the REPL until the user executes the 'quit'
runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

