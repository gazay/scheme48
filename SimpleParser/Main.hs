import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Char
import Data.Ratio
import Data.Complex

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | Rational Rational
             | Complex (Complex Double)
             | String String
             | Bool Bool
             | Character Char
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

parseFloat :: Parser LispVal
parseFloat = do
              real <- many1 digit
              char '.'
              frac <- many1 digit
              let cuttedFrac = take 6 frac
              let float = real ++ '.':cuttedFrac
              return . Float $ flt2dig float

parseLongFloat :: Parser LispVal
parseLongFloat = do
              real <- many1 digit
              char '.'
              frac <- many1 digit
              char 'L'
              prolongate <- digit
              let restFrac = replicate (12 - length frac) prolongate
              let float = real ++ '.':frac ++ restFrac
              return . Float $ flt2dig float

flt2dig :: String -> Double
flt2dig x = fst $ readFloat x !! 0

toDouble :: LispVal -> Double
toDouble (Float f) = f
toDouble (Number n) = fromIntegral n

parseRational :: Parser LispVal
parseRational = do
                  qout <- many digit
                  char '/'
                  denom <- many digit
                  let ratio = read qout % read denom
                  return . Rational $ ratio

parseComplex :: Parser LispVal
parseComplex = do
                real <- (try parseFloat <|> parseDigital1)
                char '+'
                imag <- (try parseFloat <|> parseDigital1)
                char 'i'
                let complex = toDouble real :+ toDouble imag
                return . Complex $ complex

parseCharacter :: Parser LispVal
parseCharacter = do try $ string "#\\"
                    value <- try (string "newline" <|> string "space")
                             <|> do { x <- anyChar; notFollowedBy alphaNum; return [x] }
                    return $ Character $ case value of
                                           "space" -> ' '
                                           "newline" -> '\n'
                                           otherwise -> (value !! 0)

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
                return $ List [Atom "qoute", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseComplex
        <|> try parseRational
        <|> try parseLongFloat
        <|> try parseFloat
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

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
