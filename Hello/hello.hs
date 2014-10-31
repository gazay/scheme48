module Main where
import System.Environment

main :: IO ()
main = do
    (a : b : args) <- getArgs
    putStrLn $ "Hello " ++ a ++ " " ++  b
