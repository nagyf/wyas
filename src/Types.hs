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
    Env,
    LispError(..),
    ThrowsError,
    IOThrowsError,
    liftThrows,
    runIOThrows
    ) where

import Control.Monad.Error()
import Data.IORef
import Text.ParserCombinators.Parsec (ParseError)
import Control.Monad.Error
import System.IO (Handle)

-- |Represents an error occured during the evaluation of a Lisp expression
data LispError = NumArgs Integer [LispVal] -- ^ Wrong number of arguments
               | TypeMismatch String LispVal -- ^ Wrong type
               | Parser ParseError -- ^ Parser error
               | BadSpecialForm String LispVal
               | NotFunction String String -- ^ Function was expected
               | UnboundVar String String -- ^ Unknown variable
               | Default String -- ^ Any other error message

instance Show LispError where
    show = showError

instance Error LispError where
     noMsg = Default "An unknown error has occurred..."
     strMsg = Default

-- |Partially applied 'Either' monad, with 'LispError' as first argument
type ThrowsError = Either LispError

-- |Shows the error as string
showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Default err) = err

-- |Takes any error value and converts it to it's 'String' representation
trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action $ return . show

-- |Extracts the value wrapped in 'Right'
extractValue :: ThrowsError a -> a
extractValue (Right x) = x

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

-- |Represents a Lisp value
data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
    | Func {
        parameters :: [String],
        vararg :: (Maybe String),
        body :: [LispVal],
        closue :: Env
      }
    | IOFunc ([LispVal] -> IOThrowsError LispVal)
    | Port Handle

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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func ps v _ _) =
  "(lambda (" ++ unwords (map show ps) ++ (case v of
    Nothing -> ""
    Just val -> " . " ++ val) ++ ")"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

-- |Prints every 'LispVal', and concatenates it to a 'String'
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- |Represents an Environment which stores the lisp variables
type Env = IORef [(String, IORef LispVal)]