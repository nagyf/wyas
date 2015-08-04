{-|
Module      : Parser
Description : Defines the lisp parser
License     : MIT
Maintainer  : jester.nf@gmail.com
Stability   : stable
Portability : portable

This module can be used to parse lisp expressions.
-}
module Parser(readExpr, readExprList) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Types
import Control.Monad.Error

-- |Parser for any lisp symbol
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- |Parser for any number of space
spaces :: Parser ()
spaces = skipMany1 space

-- |Escaped character parser
escaped :: Parser String
escaped = do
    f <- char '\\'
    x <- oneOf "\\nrvtbf\""
    return [f, x]

-- |Parser for any character except for escapable ones
nonEscaped :: Parser Char
nonEscaped = noneOf "\\\"\t\n\r\v\b\0"

-- |Parser for any non-escaped or escaped characters
character :: Parser String
character = fmap return nonEscaped <|> escaped

-- |Parse a 'String' value wrapped in quotes
parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many character
    char '"'
    return $ String $ concat x

-- |Parser for a lisp 'Atom'. If it's #t or #f, it returns a 'Bool' value
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom

-- |Parse a 'Number'
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- |Parser for a 'List' value
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- |Parser for a 'DottedList' value
parseDottedList :: Parser LispVal
parseDottedList = do
    h <- endBy parseExpr spaces
    t <- char '.' >> spaces >> parseExpr
    return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- |Parser for any Lisp expression
parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber
            <|> parseQuoted
            <|> do
                    char '('
                    x <- try parseList <|> parseDottedList
                    char ')'
                    return x

-- |Reads an expression from the 'String' parameter, and either returns an error
-- message, or returns the parsed Lisp expression as 'LispVal'
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)