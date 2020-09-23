module Main where

import qualified Calc.Lexer as Lex
import qualified Calc.Parser as Par
import qualified ConstraintGenerator as CG
--import qualified Importer as I
import qualified Solver as Sol
import qualified TermReducer as TR
import qualified Translator as Trans


import System.Environment
import qualified Euterpea as Eu

data Scenario = Lexing | Parsing | Importing | Constraints | Solve | Reduce | Translate


main :: IO ()
main = 
  let scenario = Translate
  in do args <- getArgs
        case args of
         [file] -> do s <- readFile file
                      showResult scenario s
         _ -> putStrLn "Wrong number of arguments"

lexM str = Lex.scanner str
parseM str = do val <- lexM str
                Par.parseTokens val
consM str  = do val <- parseM str
                return $ CG.generateConstraints val
solveM str = do val <- parseM str
                let cons = CG.generateConstraints val
                Sol.solve cons
                return val
reduceM str = do val <- solveM str
                 TR.reduce val
transM str = do val <- reduceM str
                Trans.translate val
putShow val = putStrLn $ show $ val



showResult :: Scenario -> String -> IO ()
showResult Lexing str = putShow $ lexM str
showResult Parsing str = putShow $ parseM str
showResult Constraints str = putShow $ consM str
showResult Solve str = putShow $ solveM str
showResult Reduce str = putShow $ reduceM str
showResult Translate str = case transM str of
                             Right music -> Eu.playDev 2 music
                             Left err -> putStrLn err
