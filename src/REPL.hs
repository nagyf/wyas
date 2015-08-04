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
module REPL(
    runRepl,
    evalAndPrint,
    runOne) where

import System.IO
import Control.Monad
import Evaluator
import Parser
import Types

-- |Print the prompt without a newline character, and flush the buffer
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- |Print the prompt, and read until newline character
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

-- |Repeats a monadic action, until the given boolean expression evaluates to false
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
   result <- prompt
   if predicate result
      then return ()
      else action result >> until_ predicate prompt action

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
        >>= hPutStrLn stderr

-- |Run the REPL until the user executes the 'quit'
runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

