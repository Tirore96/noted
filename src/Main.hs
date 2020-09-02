module Main where

import qualified Calc.Lexer as Lex
import qualified Calc.Parser as Par
import qualified ConstraintGenerator as CG
import qualified Importer as I
import qualified Solver as Sol
import qualified TermReducer as TR
import qualified Translator as Trans


import System.Environment
import qualified Euterpea as Eu
--import qualified Data.Map as Map
--import Control.Monad.State.Lazy

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
importM str = do val <- parseM str
                 I.do_imports val
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



--
--
--
--showResult Parsing str = let ast = do tokens <- Lex.scanner str
--                                      Par.parseTokens tokens
--                         in putStrLn $ show ast
--
--showResult Constraints str = let cons = do tokens <- Lex.scanner str
--                                           ast <- Par.parseTokens tokens
--                                           return $ CG.generateConstraints ast
--                             in putStrLn $ show cons
--
--showResult Solve str = let cons = do tokens <- Lex.scanner str
--                                     ast <- Par.parseTokens tokens
--                                     return $ CG.generateConstraints ast
--                       in let solved = do cons_val <- cons
--                                          return $ Sol.solve cons_val
--                          in putStrLn $ (show cons)++ "\n" ++ (show solved)
--
--showResult Reduce str = let maybe_ast = do tokens <- Lex.scanner str
--                                           ast <- Par.parseTokens tokens
--                                           let cons = CG.generateConstraints ast
--                                           _ <- Sol.solve cons
--                                           return ast
--                        in case maybe_ast of
--                             Right ast -> putStrLn $ show $ TR.reduce ast
--                             Left _ -> putStrLn "type checking failed"
--
--showResult Translate str = let maybe_music= do tokens <- Lex.scanner str
--                                               ast <- Par.parseTokens tokens
--                                               let cons = CG.generateConstraints ast
--                                               _ <- Sol.solve cons
--                                               reduced <-  TR.reduce ast
--                                               Trans.translate reduced
--                           in case maybe_music of
--                                Right music -> Eu.playDev 2 music
--                                Left err -> putStrLn err
--
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

--tester Lexing str = let m = do tokens <- scanner str
--                        parseTokens tokens
--             in case m of 
--                  Right a -> let state = StateData [] 0 Map.empty
--                             in let (_,newState) = runState (genConstraints a) state 
--                                in let cs = constraints newState
--                                   in let sol = solve cs
--                                      in putStrLn $ show sol
--                  Left err -> putStrLn err
                --typeCheck ast
