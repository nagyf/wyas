{-|
Module      : Evaluator
Description : The module which evaluates lisp expressions
License     : MIT
Maintainer  : jester.nf@gmail.com
Stability   : stable
Portability : portable

This module exports the 'eval' function, what can be used to evaluate a lisp
expression.
-}
{-# LANGUAGE ExistentialQuantification #-}
module Evaluator (eval, nullEnv, bindVars, primitiveBindings) where

import Control.Monad.Error
import Types
import Data.IORef

-- |Evaluates a 'LispVal' and returns either a 'LispError' or an evaluated 'LispValue'
eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval env (Atom var) = getVar env var
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", predicate, conseq, alt]) = do
    result <- eval env predicate
    case result of
        Bool False -> eval env alt
        _ -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
     eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : b)) =
     makeNormalFunc env params b >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : b)) =
     makeVarArgs varargs env params b >>= defineVar env var
eval env (List (Atom "lambda" : List params : b)) =
     makeNormalFunc env params b
eval env (List (Atom "lambda" : DottedList params varargs : b)) =
     makeVarArgs varargs env params b
eval env (List (Atom "lambda" : varargs@(Atom _) : b)) =
     makeVarArgs varargs env [] b
eval env (List (function : args)) = do
     func <- eval env function
     argVals <- mapM (eval env) args
     apply func argVals
eval _ val = throwError $ Default $ "Unable to evaluate expression: " ++ show val

-- |Applies a function to the given arguments
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc f) args = liftThrows $ f args
apply (Func ps vs b c) args =
  if num ps /= num args && vs == Nothing
    then throwError $ NumArgs (num ps) args
    else (liftIO $ bindVars c $ zip ps args) >>= bindVarArgs vs >>= evalBody

  where
    remainingArgs = drop (length ps) args
    num = toInteger . length
    evalBody env = liftM last $ mapM (eval env) b
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
      Nothing -> return env

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("eqv?", eqv),
              ("cons", cons),
              ("equal?", equal)]

-- |Creates a numerical binary operator based on a haskell function
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _           []  = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

-- |Creates a 'Bool' binary operator based on a haskell function
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                                then throwError $ NumArgs 2 args
                                else do left <- unpacker $ args !! 0
                                        right <- unpacker $ args !! 1
                                        return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop  = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

-- |Returns the first element of a list
car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)]         = return x
car [DottedList (x : _) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

-- |Returns the rest of a list, i.e. the part of the list that follows the first item
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

-- |Appends a new element to the beggining of a list
cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

-- |Returns a boolean 'LispVal' if the two arguments are equal to each other
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) &&
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left _ -> False
                                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

-- |Checks if the 2 'LispVal' parameters are equals, using the 'Unpacker'
equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- |This data type is used to unpack real values from 'LispVal'.
-- Wraps a 'LispVal' -> 'ThrowsError a' function which is used for unpacking values.
-- Defined for every type that is an instance of 'Eq', using the 'ExistentialQuantification' extension.
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

-- |Unpacks a number from the 'LispVal' argument
-- If the argument is a 'Number', it is trivial. If it's a 'String', it tries to
-- read the number value from the String. If it is a singleton list, it calls the
-- 'unpackNum' recursively.
-- Any other parameter throws an error
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

-- |Unpacks a string from the 'LispVal' parameter
-- If it's a 'String' it is trivial.
-- If it's a 'Number', converts the number to string.
-- If it's a 'Bool', converts the value to string.
-- Any other parameter throws an error.
unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

-- |Unpacks a 'Bool' value from the 'LispVal' parameter
unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

-- |Unpacks the arguments using the given 'Unpacker' and compares them
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

-- |Creates an empty 'Env'
nullEnv :: IO Env
nullEnv = newIORef []

-- |Returns 'True' if the variable is bound in the 'Env', otherwise return 'False'
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

-- |Returns the current value of the variable
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (lookup var env)

-- |Modifies the value of an already bound variable
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . (flip writeIORef value))
    (lookup var env)
  return value

-- |Defines a new variable in the environment
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

-- |Bound multiple variables
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bs = readIORef envRef >>= extendEnv bs >>= newIORef
     where extendEnv bss env = liftM (++ env) (mapM addBinding bss)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
  where
    makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

makeFunc :: (Monad m, Show a) => Maybe String -> Env -> [a] -> [LispVal] -> m LispVal
makeFunc varargs env params b = return $ Func (map show params) varargs b env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . show