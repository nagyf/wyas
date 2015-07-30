module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    deriving (Show)

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
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match " ++ show err
    Right val -> show val