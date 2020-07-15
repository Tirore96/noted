{
module Calc.Parser where
import Calc.Lexer
import qualified Data.Map as Map
import qualified Data.List
import qualified Data.Function
import Data.Ratio



}

%monad { Either String } {>>= } {return }


%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  num  {T _ TNum _}
  note {T _ TLetter _}
  '{'  {T _ TOBracket _}
  '}'  {T _ TCBracket _}
  '('  {T _ TOPara _}
  ')'  {T _ TCPara _}
  ctxLabel {T _ TCtxLabel _}
  '=' {T _ TEq _ }
  ';'  {T _ TSemi _ }
  ','  {T _ TComma _}
  '-'  {T _ TSerial _}
  '|'  {T _ TParallel _}
  dur  {T _ TDur _}
  var {T _ TVar _}
  main {T _ TMain _}
  fun {T _ TFun _}

  newline {T _ TNewLine _}

%%

Program :: {[Assignment]}
        : Assignments LastAssignment {reverse ($2:$1)}
        | LastAssignment {[$1]}


Assignments :: {[Assignment]}
        : Assignments Assignment {$2:$1}
        | Assignment {[$1]}


Assignment :: {Assignment}
        : var '=' Term newline {Assignment (parseBase $1) $3}

LastAssignment :: {Assignment}
        : var '=' Term {Assignment (parseBase $1) $3}
        | Assignment {$1}




Term :: {Term}
        : fun '(' TermArgs ')' {Application (parseFun $1) (reverse $3)} 
        | Term1 {$1}

TermArgs :: {[Term]}
          : TermArgs ',' Term {$3:$1}
          | Term {[$1]}


Term1 :: {Term}
        : Term1 '-' Term2 {Composition Serial $1 $3}
        | Term2 {$1}

Term2 :: {Term}
        : Term2 '|' Term3 {Composition Parallel $1 $3}  -- <--- Remember to add this constructor
        | Term3 {$1}
       

Term3 :: {Term}
        : num {Num (parseBaseInt $1)}
        | dur {Dur (parseDur $1)}
        | note {Letter (parseBase $1)}
        | var {Variable (parseBase $1)}
        | '{' ListPairs '}' {Context (sortPairs $2)}
        | '(' Term ')' {$2}

ListPairs :: {[(Label,Term)]}
	      : ListPairs ';' Pair {$3:$1}
	      | Pair {[$1]}

Pair :: {(Label,Term)}
	: ctxLabel '=' Term3 {(parseCtxLabel $1,$3)}
  
{
sortPairs :: [(Label,Term)] ->  [(Label,Term)]
sortPairs labels = Data.List.sortBy (flip compare `Data.Function.on` fst) labels

parseBaseInt :: Token -> (Integer,Pos)
parseBaseInt (T pos _ str) = (read str,parseAlexPosn pos)

parseFun :: Token -> (Function,Pos)
parseFun (T pos _ "toNotes") = (ToNotes,parseAlexPosn pos)
parseFun (T pos _ "toMusic") = (ToMusic,parseAlexPosn pos)

parseFun _ = undefined


durStrToRational :: String -> Integer
durStrToRational "wn" = 1
durStrToRational "hn" = 2
durStrToRational "qn" = 4
durStrToRational "en" = 8

parseDur :: Token -> (Integer,Pos)
parseDur (T pos _ str) = (durStrToRational str,parseAlexPosn pos)

parseBase :: Token -> (String,Pos)
parseBase (T pos _ str) = (str,parseAlexPosn pos)

parseLetterN (T pos _ str1) (T _ _ str2) = (str1,read str2,parseAlexPosn pos)

parseCtxLabel:: Token -> Label
parseCtxLabel(T _ TCtxLabel str) = case str of 
                                   "key" -> Key 
                                   "octave" -> Octave
                                   _  -> undefined

parseAlexPosn :: AlexPosn -> Pos
parseAlexPosn (AlexPn _ line column) = (line,column)

type Program = [Assignment]
data Assignment = Assignment (String,Pos) Term
  deriving(Eq,Show)

data CompType = Serial | Parallel
  deriving(Eq,Show)


type Pos = (Int,Int)

data Term = Num (Integer,Pos)
             | Dur (Integer,Pos)
             | Letter (String,Pos)
             | Composition CompType Term Term
             | Variable (String,Pos)
             | Context [(Label,Term)] 
             | Application (Function,Pos) [Term]
  deriving(Eq,Show)

--instance Show Term where
--  show (Num (n,_)) = show n
--  show (Dur (r,_)) = show r
--  show (Letter (s,_)) = s
--  show (Composition t t1 t2) = "("++(show t) ++ "composition" ++ (show t1) ++ ";" ++ (show t2)
--  show (Variable (s,_)) = "Variable "++ s
--  show (Context ctx)= "Context "++(show ctx) 
--  show (Application (f,_) terms ) = (show f)++" " ++ (show terms)



data Label = Bars | Key | Octave
  deriving(Eq,Show)

instance Ord Label where
  compare Bars Bars = EQ
  compare Key Key = EQ
  compare Octave Octave = EQ
  compare Bars _ = GT
  compare Key Bars = LT
  compare Key _ = GT
  compare Octave _ = LT

data Function = ToNotes | ToMusic
  deriving(Eq,Show)





parseError (first:rest)= let (T p _ s)=first
		    in let (AlexPn _ l c)=p
		       in let msg = "Parse error on line " ++ (show l) ++ ", column "++(show c)++" during parsing of "++s
			  in Left msg
parseError [] = Left "empty token list but failed"

}
