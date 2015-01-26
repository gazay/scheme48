{-# LANGUAGE ExistentialQuantification #-}

import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import Control.Monad.Error
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
             deriving (Show)

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
               deriving (Show)

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)
                  deriving (Show)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2
    `catchError` (const $ return False)

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
showVal (DottedList listHead listTail) = "(" ++ unwordsList listHead ++ " . " ++ showVal listTail ++ ")"
showVal (Vector contents) = "(" ++ show contents ++ ")"

instance Show LispVal where show = showVal

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

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
hex2dig x = fst $ readHex (head x)

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              many1 octDigit >>= (return . Number . oct2dig)

oct2dig :: String -> Integer
oct2dig x = fst $ readOct (head x)

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              many1 (oneOf "01") >>= (return . Number . bin2dig)

bin2dig :: String -> Integer
bin2dig x = fst $ readInt 2 isDigit digitToInt (head x)

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
flt2dig x = fst $ readFloat (head x)

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
                                           otherwise -> (head value)

parseAnyList :: Parser LispVal
parseAnyList = do
                char '(' >> spaces
                valHead <- parseExpr `sepEndBy` spaces
                do valTail <- char '.' >> spaces1 >> parseExpr
                   return $ DottedList valHead valTail
                  <|> (spaces >> char ')' >> (return $ List valHead))

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

eval :: LispVal -> ThrowsError LispVal
eval val@(Bool _)                          = return val
eval val@(Number _)                        = return val
eval val@(Float _)                         = return val
eval val@(Rational _)                      = return val
eval val@(Complex _)                       = return val
eval val@(Atom _)                          = return val
eval val@(String _)                        = return val
eval val@(Character _)                     = return val
-- eval val@(Vector _) = return val
eval (List [Atom "quote", val])            = return val
eval (List [Atom "quasiquote", val])       = return val
eval (List [Atom "if", pred, conseq, alt]) =
    do result <- eval pred
       case result of
         Bool False -> eval alt
         Bool True  -> eval conseq
         otherwise  -> throwError $ TypeMismatch "bool" pred
eval (List (Atom func : args))             = mapM eval args >>= apply func
eval val@(List _)                          = return val
eval val@(DottedList _ _)                  = return val
eval badForm                               = throwError $ BadSpecialForm "Unrecognized special form" badForm

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 1 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [l1@(List arg1), l2@(List arg2)] = eqvList eqv [l1, l2]
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                                 (all eqvPair $ zip arg1 arg2)
                                                 where eqvPair (x1, x2) = case eqvFunc [x1, x2] of
                                                                            Left err -> False
                                                                            Right (Bool val) -> val

equal :: [LispVal] -> ThrowsError LispVal
equal [l1@(List arg1), l2@(List arg2)] = eqvList equal [l1, l2]
equal [(DottedList xs x), (DottedList ys y)] = eqvList equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                                       [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+",              numericBinop (+)),
              ("-",              numericBinop (-)),
              ("*",              numericBinop (*)),
              ("/",              numericBinop div),
              ("mod",            numericBinop mod),
              ("quotinent",      numericBinop quot),
              ("remainder",      numericBinop rem),
              ("=",              numBoolBinop (==)),
              ("<",              numBoolBinop (<)),
              (">",              numBoolBinop (>)),
              ("/=",             numBoolBinop (/=)),
              ("<=",             numBoolBinop (<=)),
              (">=",             numBoolBinop (>=)),
              ("&&",             boolBoolBinop (&&)),
              ("||",             boolBoolBinop (||)),
              ("string=?",       strBoolBinop (==)),
              ("string<?",       strBoolBinop (<)),
              ("string>?",       strBoolBinop (>)),
              ("string<=?",      strBoolBinop (<=)),
              ("string>=?",      strBoolBinop (>=)),
              ("symbol?",        typeOp atomP),
              ("list?",          typeOp listP),
              ("string?",        typeOp stringP),
              ("number?",        typeOp numberP),
              ("float?",         typeOp floatP),
              ("complex?",       typeOp complexP),
              ("bool?",          typeOp boolP),
              ("ratinal?",       typeOp rationalP),
              ("character?",     typeOp characterP),
              ("vector?",        typeOp vectorP),
              ("symbol->string", symbol2string),
              ("string->symbol", string2symbol),
              ("car",            car),
              ("cdr",            cdr),
              ("eq?",            eqv),
              ("eqv?",           eqv),
              ("equal?",         equal)]

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                               then throwError $ NumArgs 2 args
                               else do left <- unpacker $ head args
                                       right <- unpacker $ args !! 1
                                       return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []              = throwError $ NumArgs 2 []
numericBinop op singleValue@[_] = throwError $ NumArgs 2 singleValue
numericBinop op params          = mapM unpackNum params >>= return . Number . foldl1 op

typeOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
typeOp f []  = throwError $ NumArgs 2 []
typeOp f [v] = return $ f v

symbol2string, string2symbol :: [LispVal] -> ThrowsError LispVal
symbol2string [(Atom a)]     = return (String a)
symbol2string [notSymbol]    = throwError $ TypeMismatch "symbol" notSymbol
symbol2string []             = throwError $ NumArgs 1 []
string2symbol [(String a)]   = return (Atom a)
string2symbol [notString]    = throwError $ TypeMismatch "string" notString
string2symbol []             = throwError $ NumArgs 1 []

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number val) = return val
unpackNum (String val) = let parsed = reads val in
                             if null parsed
                               then throwError $ TypeMismatch "number" $ String val
                               else return . fst $ head parsed
unpackNum (List [val]) = unpackNum val
unpackNum notNum = throwError $ TypeMismatch "number" notNum

atomP, stringP, numberP, complexP, floatP, rationalP, listP, characterP, boolP, vectorP :: LispVal -> LispVal
atomP       (Atom _)         = Bool True
atomP       _                = Bool False
stringP     (String _)       = Bool True
stringP     _                = Bool False
numberP     (Number _)       = Bool True
numberP     _                = Bool False
complexP    (Complex _)      = Bool True
complexP    _                = Bool False
rationalP   (Rational _)     = Bool True
rationalP   _                = Bool False
floatP      (Float _)        = Bool True
floatP      _                = Bool False
listP       (List _)         = Bool True
listP       (DottedList _ _) = Bool True
listP       _                = Bool False
boolP       (Bool _)         = Bool True
boolP       _                = Bool False
characterP  (Character _)    = Bool True
characterP  _                = Bool False
vectorP     (Vector _)       = Bool True
vectorP     _                = Bool False

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err  -> throwError $ Parser err
                   Right val -> return val

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (head args) >>= eval
    putStrLn $ extractValue $ trapError evaled
