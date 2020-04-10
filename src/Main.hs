module Main where

import Calc
import System.Environment
import TypeChecker
import Evaluator
import Euterpea as Eu

main :: IO ()
main = do return ()
--main = do
--  args <- getArgs;
--  case args of
--    [file] -> do
--	s <- readFile file
--    	sendMIDI s
--    _ -> putStrLn "Wrong number of arguments"
--
--sendMIDI :: String -> IO ()
--sendMIDI program =  let result = do tokenized <- (scanner program)
--                                    exp <- parseTokens tokenized 
--                                    typeCheck exp 
--                                    evaluate exp
--                    in case result of 
--                       Right music -> Eu.playDev 2 music :: ToMusic1 (AbsPitch,Volume)
--                       Left error -> putStrLn error


tester str = do tokens <- scanner str
                ast <- parseTokens tokens
                typeCheck ast
