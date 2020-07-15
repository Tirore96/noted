module Main where

import Calc
import System.Environment
import TypeChecker
import Evaluator
import Euterpea as Eu

main :: IO ()
main = do
  args <- getArgs
  case args of
   [file] -> do s <- readFile file
                sendMIDI s
             --do s <- readFile file 
             --   case tester s of 
             --     Right ast -> putStrLn ast
             --     Left s -> putStrLn s
   _ -> putStrLn "Wrong number of arguments"

sendMIDI :: String -> IO ()
sendMIDI program = 
 let result = do
            tokenized <- (scanner program) 
            ast <- parseTokens tokenized 
            typedExp <- typeCheck ast 
            evaluate ast 
 in case result of 
      Right music -> Eu.playDev 2 music 
      Left error -> putStrLn error


-- ToMusic1 (AbsPitch,Volume)

--tester str = do tokens <- scanner str
--                ast <- parseTokens tokens
--                typeCheck ast
--                --typeCheck ast
