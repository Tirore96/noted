{
module Calc.Parser where
import Calc.Lexer
import qualified Data.Map as Map



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
        : var '=' Term Exp {Assign (parseBase $1) $3 $4}
        | Term {Term $1}

Term :: {Term}
        : Term1 '(' Term1 ')' {Application $1 $3}
        | Term1 '{' ListPairs '}' {Application $1 (Context $3)}
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
parseBaseInt :: Token -> (Integer,Pos)
parseBaseInt (T pos _ str) = (read str,parseAlexPosn pos)

parseBase :: Token -> (String,Pos)
parseBase (T pos _ str) = (str,parseAlexPosn pos)

parseNoteN (T pos _ str1) (T _ _ str2) = (str1,read str2,parseAlexPosn pos)

parseCtxWord :: Token -> Label
parseCtxWord (T _ TCtxWord str) = case str of 
                                   "bars" -> Bars
                                   "key" -> Key 
                                   "time" -> Time
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

data Label = Bars | Key | Time | OctavePos 
  deriving(Eq,Show,Ord)

data CtxValue = CNum (Integer,Pos) | CNote (String,Pos) | Signature (Integer, Integer,Pos)
  deriving(Eq,Show)



parseError tokens = let (T p _ s)=(head tokens)
		    in let (AlexPn _ l c)=p
		       in let msg = "Parse error on line " ++ (show l) ++ ", column "++(show c)++" during parsing of "++s
			  in Left msg
parseError [] = Left "empty token list but failed"

}
