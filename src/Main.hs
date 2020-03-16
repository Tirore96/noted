module Main where

import Calc
import System.Environment

main :: IO ()
main = do
  args <- getArgs;
  case args of
    [file] -> do
	s <- readFile file
    	let tokenized = alexScanTokens s;
    	putStrLn $ show tokenized
    _ -> putStrLn "Wrong number of arguments"

