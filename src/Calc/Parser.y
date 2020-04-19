{
module Calc.Parser where
import Calc.Lexer
import qualified Data.Map as Map
import qualified Data.List
import qualified Data.Function



}

%monad { Either String } {>>= } {return }


%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  num  {T _ TNum _}
  note {T _ TNote _}
  chord {T _ TChord _ }
  '{'  {T _ TOBracket _}
  '}'  {T _ TCBracket _}
  '('  {T _ TOPara _}
  ')'  {T _ TCPara _}
  ctxWord {T _ TCtxWord _}
  ctxNum {T _ TCtxNum _}
  ctxKey {T _ TCtxNote _}
  ctxSig {T _ TCtxNote _}
 xSlash {T _ TCtxSlash _ }

  '=' {T _ TEq _ }
  ',' {T _ TComma _}
  '.' {T _ TDot _}
  ';'  {T _ TSemi _ }
  var {T _ TVar _}
  space {T _ TSpace _}
  newline {T _ TNewLine _}

--  eof	   {T _ TEOF _}
--  '/' {T _ TDiv _}
--  '+' {T _ TPlus _}
--  '-' {T _ TMinus _}
--  '#' {T _ TSharp _}
--  minor {T _ TMin _}
--  flat {T _ TFlat _}
--  '_' {T _ TUnderscore _}
--  R {T _ TR _ }

%%


Exp :: {Exp}
        : var space '=' space Term newline Exp {Assign (parseBase $1) $5 $7}
        | var '=' space Term newline Exp {Assign (parseBase $1) $4 $6}
        | var space '=' Term newline Exp {Assign (parseBase $1) $4 $6}
        | var '=' Term newline Exp {Assign (parseBase $1) $3 $5}



        | Term newline {Term $1}

--Term :: {Term}
--        : Term1 '(' Term1 ')' {Application $1 $3}
--        | Term1 '{' ListPairs '}' {Application $1 (Context (sortPairs $3))}
--        | Term1 {$1}

Term :: {Term}
        : Term1 space Term {Application $1 $3}
        | Term1 {$1}


Term1 :: {Term}
        : Term1 ',' Term2 {Commaed $1 $3}
        | Term2 {$1}

Term2 :: {Term}
        : num {Num (parseBaseInt $1)}
        | note {Note (parseBase $1)}
        | note num {NoteN (parseNoteN $1 $2)}
        | Term2 '.' {Dotted $1}
        | Term2 Term2 {Concatted $1 $2}
        | var {Variable (parseBase $1)}
        | '{' ListPairs '}' {Context (sortPairs $2)}
--        | '(' Term3 ')' {$2}

--Term3 :: {Term}
--        : Term1 space ',' space Term3 {Commaed $1 $5}
--        | Term1 ',' space Term3 {Commaed $1 $4}
--        | Term1 space ',' Term3 {Commaed $1 $4}
--        | Term1 ',' Term3 {Commaed $1 $3}
--        | Term4 {$1}
--
--Term4 :: {Term}
--        : Term1 space Term4 {Concatted $1 $3}
--        | Term1 Term4 {Concatted $1 $2}
--        | Term2 {$1}


ListPairs :: {[(Label,CtxValue)]}
	      : ListPairs ';' Pair {$3:$1}
	      | Pair {[$1]}

Pair :: {(Label,CtxValue)}
	: ctxWord '=' CtxValue {(parseCtxWord $1,$3)}
CtxValue :: {CtxValue}
         : ctxNum    {parseCNum $1}
         | ctxKey    {parseCNote $1}
         | ctxNum xSlash ctxNum {parseSig $1 $3}
{
sortPairs :: [(Label,CtxValue)] ->  [(Label,CtxValue)]
sortPairs labels = Data.List.sortBy (flip compare `Data.Function.on` fst) labels
parseBaseInt :: Token -> (Integer,Pos)
parseBaseInt (T pos _ str) = (read str,parseAlexPosn pos)

parseBase :: Token -> (String,Pos)
parseBase (T pos _ str) = (str,parseAlexPosn pos)

parseNoteN (T pos _ str1) (T _ _ str2) = (str1,read str2,parseAlexPosn pos)

parseCtxWord :: Token -> Label
parseCtxWord (T _ TCtxWord str) = case str of 
                                   "bars" -> Bars
                                   "key" -> Key 
                                   "octave_pos" -> OctavePos

                                   _  -> undefined

parseAlexPosn :: AlexPosn -> Pos
parseAlexPosn (AlexPn _ line column) = (line,column)

parseCNum (T pos _ str) = CNum (read str,parseAlexPosn pos)
parseCNote (T pos _ str) = CNote (str,parseAlexPosn pos)
parseSig (T pos _ str1) (T _ _ str2) = Signature (read str1,read str2,parseAlexPosn pos)




----------------------------------------------

data Exp = Assign (String,Pos) Term Exp | Term Term 
  deriving(Eq,Show)

type Pos = (Int,Int)

data Term = Num (Integer,Pos)
             | Note (String,Pos)
             | NoteN (String,Integer,Pos)
             | Dotted Term 
             | Concatted Term Term
             | Commaed Term Term
             | Variable (String,Pos)
             | Context [(Label,CtxValue)] 
             | Application Term Term
  deriving(Eq,Show)

data Label = Bars | Key | OctavePos 
  deriving(Eq,Show)

instance Ord Label where
  compare Bars Bars = EQ
  compare Key Key = EQ
  compare OctavePos OctavePos = EQ
  compare Bars _ = GT
  compare Key Bars = LT
  compare Key _ = GT
  compare OctavePos _ = LT



data CtxValue = CNum (Integer,Pos) | CNote (String,Pos) | Signature (Integer, Integer,Pos)
  deriving(Eq,Show)



parseError (first:rest)= let (T p _ s)=first
		    in let (AlexPn _ l c)=p
		       in let msg = "Parse error on line " ++ (show l) ++ ", column "++(show c)++" during parsing of "++s
			  in Left msg
parseError [] = Left "empty token list but failed"

}
