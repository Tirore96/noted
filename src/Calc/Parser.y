{
module Calc.Parser where
import qualified Calc.Lexer as Lex
import qualified Data.Map as Map
import qualified Data.List
import qualified Data.Function
import Data.Ratio
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except


}

%monad { Either String } {>>= } {return }


%name parseTokens
%tokentype { Lex.Token }
%error { parseError }

%token
   newline {Lex.T _ Lex.TNewLine _}
   '(' {Lex.T _ Lex.TOPara _}
   ')'  {Lex.T _ Lex.TCPara _}
   '='  {Lex.T _ Lex.TEq _}
   var {Lex.T _ Lex.TVar _}
   '|' {Lex.T _ Lex.TParallel _}
   '-' {Lex.T _ Lex.TSerial _}
   dur {Lex.T _ Lex.TDur _}
   octave {Lex.T _ Lex.TOctave _}
   letter {Lex.T _ Lex.TLetter _}
   color {Lex.T _ Lex.TColor _}
   index {Lex.T _ Lex.TIndex _}
   withDur {Lex.T _  Lex.TWithDur _}
   withOctave {Lex.T _  Lex.TWithOctave _}
   withScale {Lex.T _ Lex.TWithScale _}
   withColor {Lex.T _  Lex.TWithColor _}
   import    {Lex.T _ Lex.TImport _}
   importname {Lex.T _ Lex.TImportName _}
   eof {Lex.T _ Lex.TEOF _}

%left withDur
%left withOctave 
%left withScale 
%left withColor
%left '-'
%left '|'





%%

Program :: {[Line]}
        : Lines {reverse $1}

Lines :: {[Line]}
        : Lines Line {$2:$1}
        | Line {[$1]}

Line :: {Line}
        : import importname   {Import (parseImportName $2,tokenPos $1)}
        | var '=' TermImport newline {Assignment (parseBase $1) $3}

--remember newline check later
TermImport :: {TermImport}
        : TermImport withDur TermImport     {TIWith ((WithDur,$1,$3),getPos $1) }
        | TermImport withOctave TermImport  {TIWith ((WithOctave,$1,$3),getPos $1) }
        | TermImport withScale TermImport   {TIWith ((WithScale, $1,$3),getPos $1) }
        | TermImport withColor TermImport   {TIWith ((WithColor,$1,$3),getPos $1) }
        | TermImport '-' TermImport   {buildFlatList Serial $1 $3}
        | TermImport '|' TermImport   {buildFlatList Parallel $1 $3}
        | index {TIIndex (parseIndex $1)}
        | dur {TIDur (parseDur $1)}
        | letter {TILetter (parseLetter $1)}
        | octave {TIOctave (parseOctave $1)}
        | color {TIColor (parseColor $1)}
        | var {TIVar (parseBase $1)}
        | '(' TermImport ')' {$2}

{


data ColorType = Major | Minor
  deriving(Eq,Ord,Show)

data WithType = WithDur | WithOctave | WithScale | WithColor
  deriving(Eq,Ord,Show)

--data Letter = A | B | C | D | E | F | G
--  deriving(Eq,Ord,Show)
--
--data Sign = Flat | Sharp | Natural
--  deriving(Eq,Ord,Show)


type Pos = (Int,Int)
data TermImport =  TIIndex (Integer,Pos)
                | TIDur (Integer,Pos)
                | TILetter (Integer,Pos)
                | TIOctave (Integer,Pos)
                | TIColor (ColorType,Pos)
                | TIFlatList ((CompType,[TermImport]),Pos)
                | TIVar (String,Pos)
                | TIWith ((WithType,TermImport,TermImport),Pos)
  deriving(Eq,Ord)


data Term =  TIndex (Integer,Pos)
                | TDur (Integer,Pos)
                | TLetter (Integer,Pos)
                | TOctave (Integer,Pos)
                | TColor (ColorType,Pos)
                | TFlatList ((CompType,[Term]),Pos)
                | TVar (String,Pos)
                | TWith ((WithType,Term,Term),Pos)
  deriving(Eq,Ord)

load_module :: String -> IO (Either String Program)
load_module str = runExcept $ do
  tokens <- Lex.Scanner tokens
  ast <- parseTokens tokens
  liftIO ast
  

parseTokensImport tokens = case parseTokens tokens of
                             Right program -> concatMapM processLine program
                             Left err -> Left err

concatMapM fun l = do mapped_l <- mapM fun l
                      return $ foldl (++) [] mapped_l 


processLine Line -> Program
processLine line@(Assignment _ _) = return [Right line]
processLine (Import file) = do 
  str <- readFile (fst file)
  case Lex.scanner str of
    Right tokens -> case parseTokensImport tokens of
                      Right program -> Right program
    Left err -> return $ putStrLn err 
 


getPos :: TermImport -> Pos
getPos (TIIndex (_,p)) = p
getPos (TIDur (_,p)) = p
getPos (TILetter (_,p)) = p
getPos (TIOctave (_,p)) = p
getPos (TIColor (_,p)) = p
getPos (TIFlatList (_,p)) = p
getPos (TIVar (_,p)) = p
getPos (TIWith (_,p)) = p

tokenPos :: Lex.Token -> Pos
tokenPos (Lex.T pos _ _) = parseAlexPosn pos

parseImportName :: Lex.Token -> String
parseImportName (Lex.T pos _ str) = str

parseAlexPosn :: Lex.AlexPosn -> Pos
parseAlexPosn (Lex.AlexPn _ line column) = (line,column)

parseIndex :: Lex.Token -> (Integer,Pos)
parseIndex (Lex.T pos _ str) = 
  let num_str = init (init str)
  in ((read num_str)-1,parseAlexPosn pos) --subtract 1 to make index zero_indexing

parseDur :: Lex.Token -> (Integer,Pos)
parseDur (Lex.T pos _ str) = 
  let num = case str of
              "wn" -> 1
              "hn" -> 2
              "qn" -> 4
              "en" -> 8
              _ -> undefined
  in (num,parseAlexPosn pos)


parseBase :: Lex.Token -> (String,Pos)
parseBase (Lex.T pos _ str) = (str,parseAlexPosn pos)

parseLetter (Lex.T pos _ str) = 
  let letter_index = case (head str) of
                       'A' -> 0
                       'B' -> 2
                       'C' -> 3
                       'D' -> 5
                       'E' -> 7
                       'F' -> 8
                       'G' -> 10
                       _ -> undefined
      sign_index = case (last str) of
                     '#' -> 1
                     'b' -> -1
                     _ -> 0

  in (letter_index+sign_index,parseAlexPosn pos)


parseOctave (Lex.T pos _ str) =
  let num = read (tail str) 
  in (num,parseAlexPosn pos)

parseColor (Lex.T pos _ str) =
  let color = case str of
                "Major" -> Major
                "Minor" -> Minor
                _ -> undefined
  in (color,parseAlexPosn pos)


type Program = [Line]
data Line = Assignment (String,Pos) TermImport | Import (String,Pos)
  deriving(Eq)

instance Show Line where
  show (Assignment (s,p) term )= s ++ "=" ++ (show term)
  show (Import (s,p) )= "import "++s


data CompType = Serial | Parallel
  deriving(Eq,Ord)


---show


instance Show CompType where
  show Serial = "-"
  show Parallel = "|"





buildFlatList :: CompType -> TermImport -> TermImport -> TermImport
buildFlatList compType t1 t2 = 
  let makeFlatListMember flat_notes = case flat_notes of
                                         TFlatList ((compType1,l),_) -> 
                                              if compType == compType1 
                                                then l
                                                else [flat_notes]
                                         base -> [base]
  in let t1_member = makeFlatListMember t1
         t2_member = makeFlatListMember t2
     in TFlatList ((compType,(t1_member++t2_member)),getPos t1)


instance Show TermImport where
  show (TIIndex (n,_)) = "(TIndex " ++ (show n)++")"
  show (TIDur (8,_)) = "en"
  show (TIDur (4,_)) = "qn"
  show (TIDur (2,_)) = "hn"
  show (TIDur (1,_)) = "wn"
  show (TILetter (l,_)) = show l
  show (TIOctave (n,_)) = "(TOctave " ++ (show n) ++")"
  show (TIColor (c,_)) = show c
  show (TIColor (c,_)) = show c
  show (TIFlatList ((t,[last]),_)) = (show last) 
  show (TIFlatList ((t,(first:rest)),_)) = (show first) ++ (show t) ++  (show (TFlatList ((t,rest),undefined) ))
  show (TIVar (s,_)) = "(TVar "++s++")"
  show (TIWith ((tW,t1,t2),_)) = (show t1) ++" " ++ (show tW) ++" " ++  (show t2)



parseError (first:rest)= let (Lex.T p _ s)=first
		    in let (Lex.AlexPn _ l c)=p
		       in let msg = "Parse error on line " ++ (show l) ++ ", column "++(show c)++" during parsing of "++s
			  in Left msg
parseError [] = Left "empty token list but failed"

}
