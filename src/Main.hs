module Main where

import Calc
import System.Environment
import TypeChecker
--import Evaluator
import qualified Euterpea as Eu
import qualified Data.Map as Map
import Control.Monad.State.Lazy


debug = True

main :: IO ()
main = do
  args <- getArgs
  case args of
   [file] -> do s <- readFile file
                tester s
--                if debug 
--                  then tester s
--                  else sendMIDI s

             --do s <- readFile file 
             --   case tester s of 
             --     Right ast -> putStrLn ast
             --     Left s -> putStrLn s
   _ -> putStrLn "Wrong number of arguments"

--sendMIDI :: String -> IO ()
--sendMIDI program = 
-- let result = do
--            tokenized <- (scanner program) 
--            ast <- parseTokens tokenized 
--            typedExp <- typeCheck ast 
--            --evaluate ast 
-- in case result of 
--      Right music -> Eu.playDev 2 music 
--      Left error -> putStrLn error


-- ToMusic1 (AbsPitch,Volume)

tester str = let m = do tokens <- scanner str
                        parseTokens tokens
             in case m of 
                  Right a -> let state = StateData [] 0 Map.empty
                             in let (_,newState) = runState (genConstraints a) state 
                                in let cs = constraints newState
                                   in let sol = solve cs
                                      in putStrLn $ show sol
                  Left err -> putStrLn err
                --typeCheck ast
