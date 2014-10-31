module Main where
import System.Environment

main :: IO ()
main = do
    putStrLn "Please enter your name:"
    line <- getLine
    putStrLn $ "Hello " ++ line
