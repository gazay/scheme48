module Main where
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "Result is " ++ (show $ parseArgs args)

parseArgs :: [String] -> Integer
parseArgs (a : op : b : [])
  | op == "+" = num1 + num2
  | op == "*" = num1 * num2
  | op == "-" = num1 - num2
  | op == "/" = num1 `div` num2
  | otherwise = 0
    where num1 = read a :: Integer
          num2 = read b :: Integer
parseArgs _ = 0
