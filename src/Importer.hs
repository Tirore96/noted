module Importer where
import System.IO
import qualified Calc.Lexer as L
import qualified Calc.Parser as P

do_imports :: P.Program -> Either String P.Program
do_imports ((P.Import mod):rest) = do
  mod_str <- readFile (fst mod)
  let res = do tokens <- L.scanner mod_str
               new_program <- P.parseTokens tokens
               rest_imported <- do_imports rest
               return $ new_program++rest_imported
  res

do_imports (a@_:rest) = do
  rest_imported <- do_imports rest
  return $ a : rest_imported

do_imports [] = return [] 

