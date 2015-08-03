module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Types
import Control.Monad.Error
import qualified Errors as E

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- |Escaped character parser
escaped :: Parser String
escaped = do
    f <- char '\\'
    x <- oneOf "\\nrvtbf\""
    return [f, x]

escape :: Char -> String
escape ch = ['\\', ch]

nonEscaped :: Parser Char
nonEscaped = noneOf "\\\"\t\n\r\v\b\0"

character :: Parser String
character = fmap return nonEscaped <|> escaped

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many character
    char '"'
    return $ String $ concat x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

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



parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

readExpr :: String -> E.ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ E.Parser err
    Right val -> return val