{-|
Module      : Types
Description : Simple module defining basic types
License     : MIT
Maintainer  : jester.nf@gmail.com
Stability   : stable
Portability : portable
-}
module Types(
    LispVal(..),
    unwordsList,
    Env
    ) where

import Control.Monad.Error()
import Data.IORef

-- |Represents a Lisp value
data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool

instance Show LispVal where
    show = showVal

-- |Returns the 'LispVal' as 'String'
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++  ")"

-- |Prints every 'LispVal', and concatenates it to a 'String'
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- |Represents an Environment which stores the lisp variables
type Env = IORef [(String, IORef LispVal)]