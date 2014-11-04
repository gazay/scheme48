import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import Numeric
import Data.Char
import Data.Ratio
import Data.Complex
import Data.Array

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
             | Vector (Array Int LispVal)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space

escapedChar :: Parser Char
escapedChar = do
                char '\\'
                x <- oneOf "\"\r\n\t\\"
                return x

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Complex contents) = show (realPart contents) ++ " + " ++ show (imagPart contents) ++ "i"
showVal (Rational contents) = show (numerator contents) ++ "/" ++ show (denominator contents)
showVal (Bool True) = "#t"
showVal (Character contents) = "#\\" ++ [contents]
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Vector contents) = "(" ++ show contents ++ ")"

instance Show LispVal where show = showVal

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

parseAnyList :: Parser LispVal
parseAnyList = do
                char '(' >> spaces
                head <- parseExpr `sepEndBy` spaces
                do tail <- char '.' >> spaces1 >> parseExpr
                   return $ DottedList head tail
                  <|> (spaces >> char ')' >> (return $ List head))

parseQuoted :: Parser LispVal
parseQuoted = do
                char '\''
                x <- parseExpr
                return $ List [Atom "qoute", x]

parseQuasiquote :: Parser LispVal
parseQuasiquote = do
                    char '`'
                    x <- parseExpr
                    return $ List [Atom "quasiqoute", x]

parseUnquote :: Parser LispVal
parseUnquote = do
                    char ','
                    x <- parseExpr
                    return $ List [Atom "unqoute", x]

-- First implementation with Data.Array
parseVector :: Parser LispVal
parseVector = do
                try $ string "#("
                xs <- sepBy parseExpr spaces1
                char ')'
                let arr = array (1, length xs) ([(i, x) | (i, x) <- zip [1..(length xs)] xs])
                return $ Vector arr

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
        <|> try parseVector
        <|> parseQuoted
        <|> parseQuasiquote
        <|> parseUnquote
        <|> parseAnyList

eval :: LispVal -> LispVal
eval val@(Bool _) = val
eval val@(Number _) = val
eval val@(Atom _) = val
eval val@(String _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval val@(_) = String $ "Evaluating of \"" ++ show val ++ "\" Not implemented yet"

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotinent", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?",     typeOp atomP),
              ("list?",       typeOp listP),
              ("dottedList?", typeOp dottedListP),
              ("string?",     typeOp stringP),
              ("number?",     typeOp numberP),
              ("float?",      typeOp floatP),
              ("complex?",    typeOp complexP),
              ("bool?",       typeOp boolP),
              ("ratinal?",    typeOp rationalP),
              ("character?",  typeOp characterP),
              ("vector?",     typeOp characterP)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop func args = Number $ foldl1 func $ map unpackNum args

unpackNum :: LispVal -> Integer
unpackNum (Number val) = val
unpackNum _ = 0

typeOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
typeOp f [v] = f v
typeOp _ _ = Bool False

atomP, stringP, numberP, complexP, floatP, rationalP, listP, dottedListP, characterP, boolP, vectorP :: LispVal -> LispVal
atomP (Atom _) = Bool True
atomP _ = Bool False
stringP (String _) = Bool True
stringP _ = Bool False
numberP (Number _) = Bool True
numberP _ = Bool False
complexP (Complex _) = Bool True
complexP _ = Bool False
rationalP (Rational _) = Bool True
rationalP _ = Bool False
floatP (Float _) = Bool True
floatP _ = Bool False
listP (List _) = Bool True
listP _ = Bool False
dottedListP (DottedList _ _) = Bool True
dottedListP _ = Bool False
boolP (Bool _) = Bool True
boolP _ = Bool False
characterP (Character _) = Bool True
characterP _ = Bool False
vectorP (Vector _) = Bool True
vectorP _ = Bool False

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err  -> String $ "No match: " ++ show err
                   Right val -> val

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
