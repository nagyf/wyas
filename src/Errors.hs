module Errors (
  LispError(..),
  ThrowsError(..),
  trapError,
  extractValue
  ) where

import Text.ParserCombinators.Parsec (ParseError)
import Control.Monad.Error
import Types

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
    show = showError

instance Error LispError where
     noMsg = Default "An unknown error has occurred..."
     strMsg = Default

type ThrowsError = Either LispError

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

trapError action = catchError action $ return . show

extractValue :: ThrowsError a -> a
extractValue (Right x) = x