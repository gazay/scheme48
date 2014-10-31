import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err  -> "No match: " ++ show err
                   Right val -> "Found value"

escapedChar :: Parser Char
escapedChar = do
                char '\\'
                x <- oneOf "\"rnt\\"
                return x

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (escapedChar <|> noneOf "\"")
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
-- parseNumber = do
--                 x <- many1 digit
--                 let number = read x
--                 return $ Number number
-- parseNumber = do
--                 x <- many1 digit
--                 (return . Number . read) x
parseNumber = many1 digit >>= (return . Number . read)

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

spaces :: Parser ()
spaces = skipMany1 space

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ readExpr (args !! 0)
