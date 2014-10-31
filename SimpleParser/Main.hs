import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Char

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character String
             deriving (Show)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

escapedChar :: Parser Char
escapedChar = do
                char '\\'
                x <- oneOf "\"\r\n\t\\"
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

parseBool :: Parser LispVal
parseBool = do
              char '#'
              (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseNumber :: Parser LispVal
parseNumber = parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin

parseDigital1 :: Parser LispVal
parseDigital1 = many1 digit >>= (return . Number . read)

parseDigital2 :: Parser LispVal
parseDigital2 = do try $ string "#d"
                   many1 digit >>= (return . Number . read)

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              many1 hexDigit >>= (return . Number . hex2dig)

hex2dig :: String -> Integer
hex2dig x = fst $ readHex x !! 0

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              many1 octDigit >>= (return . Number . oct2dig)

oct2dig :: String -> Integer
oct2dig x = fst $ readOct x !! 0

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              many1 (oneOf "01") >>= (return . Number . bin2dig)

bin2dig :: String -> Integer
bin2dig x = fst $ readInt 2 isDigit digitToInt x !! 0

parseCharacter :: Parser LispVal
parseCharacter = do
                  char '#'
                  char '\\'
                  x <- many (letter <|> space)
                  return . Character $ "#\\" ++ x

--        <|> parseCharacter
parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseBool

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err  -> "No match: " ++ show err
                   Right val -> "Found value: " ++ show val

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ readExpr (args !! 0)
