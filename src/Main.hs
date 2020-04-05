module Main where

import Calc
import System.Environment
import TypeChecker
import Evaluator
import Euterpea as Eu

main :: IO ()
main = do
  args <- getArgs;
  case args of
    [file] -> do
	s <- readFile file
    	let eitherTokenized = scanner s
            result = do tokenized <- eitherTokenized
                        exp <- parseTokens tokenized 
                        typeCheck exp 
                        evaluate exp
            in case result of 
               Right music -> Eu.playDev 2 music
               Left error -> putStrLn error
    _ -> putStrLn "Wrong number of arguments"
--main = do
--  args <- getArgs;
--  case args of
--    [file] -> do --s <- readFile file
--                 putStrLn "hi"
--    	--case scanner s of
        --  Right x -> putStrLn (show x)
        --  Left l -> putStrLn l
    _ -> putStrLn "Wrong numbfffer of arguments"

