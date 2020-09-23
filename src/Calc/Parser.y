{
module Calc.Parser where
import qualified Calc.Lexer as Lex
import qualified Data.Map as Map
import qualified Data.List
import qualified Data.Function
import Data.Ratio



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
        | var '=' Term newline {Assignment (parseBase $1) $3}

--remember newline check later
Term :: {Term}
        : Term withDur Term     {TWith ((WithDur,$1,$3),getPos $1) }
        | Term withOctave Term  {TWith ((WithOctave,$1,$3),getPos $1) }
        | Term withScale Term   {TWith ((WithScale, $1,$3),getPos $1) }
        | Term withColor Term   {TWith ((WithColor,$1,$3),getPos $1) }
        | Term '-' Term   {buildFlatList Serial $1 $3}
        | Term '|' Term   {buildFlatList Parallel $1 $3}
        | index {TIndex (parseIndex $1)}
        | dur {TDur (parseDur $1)}
        | letter {TLetter (parseLetter $1)}
        | octave {TOctave (parseOctave $1)}
        | color {TColor (parseColor $1)}
        | var {TVar (parseBase $1)}
        | '(' Term ')' {$2}

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
data Term =  TIndex (Integer,Pos)
             | TDur (Integer,Pos)
             | TLetter (Integer,Pos)
             | TOctave (Integer,Pos)
             | TColor (ColorType,Pos)
             | TFlatList ((CompType,[Term]),Pos)
             | TVar (String,Pos)
             | TWith ((WithType,Term,Term),Pos)
  deriving(Eq,Ord)

getPos :: Term -> Pos
getPos (TIndex (_,p)) = p
getPos (TDur (_,p)) = p
getPos (TLetter (_,p)) = p
getPos (TOctave (_,p)) = p
getPos (TColor (_,p)) = p
getPos (TFlatList (_,p)) = p
getPos (TVar (_,p)) = p
getPos (TWith (_,p)) = p

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
data Line = Assignment (String,Pos) Term | Import (String,Pos)
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





buildFlatList :: CompType -> Term -> Term -> Term
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


instance Show Term where
  show (TIndex (n,_)) = "(TIndex " ++ (show n)++")"
  show (TDur (8,_)) = "en"
  show (TDur (4,_)) = "qn"
  show (TDur (2,_)) = "hn"
  show (TDur (1,_)) = "wn"
  show (TLetter (l,_)) = show l
  show (TOctave (n,_)) = "(TOctave " ++ (show n) ++")"
  show (TColor (c,_)) = show c
  show (TColor (c,_)) = show c
  show (TFlatList ((t,[last]),_)) = (show last) 
  show (TFlatList ((t,(first:rest)),_)) = (show first) ++ (show t) ++  (show (TFlatList ((t,rest),undefined) ))
  show (TVar (s,_)) = "(TVar "++s++")"
  show (TWith ((tW,t1,t2),_)) = (show t1) ++" " ++ (show tW) ++" " ++  (show t2)



parseError (first:rest)= let (Lex.T p _ s)=first
		    in let (Lex.AlexPn _ l c)=p
		       in let msg = "Parse error on line " ++ (show l) ++ ", column "++(show c)++" during parsing of "++s
			  in Left msg
parseError [] = Left "empty token list but failed"

}
