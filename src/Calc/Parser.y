{
module Calc.Parser where

import Calc.Lexer
}

--%monad { E } { thenE } { returnE }




--tree



%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  num  {TNum  $$}
  note {TNote $$}
  chord {TChord $$}
  '{'  {TOBracket $$}
  '}'  {TCBracket $$}
  ctxWord {TCtxWord $$}
  '=' {TEq $$}
  inputK {TInputK $$}
  chordK {TChordK $$}
  '"' {TQuote $$}
  ',' {TComma $$}
  '.' {TDot $$}
  '/' {TDiv $$}
  '+' {TPlus $$}
  '-' {TMinus $$}
  '#' {TSharp $$}
  minor {TMin $$}
  flat {TFlat $$}
  '_' {TUnderscore $$}
  R {TR $$}
  var {TVar $$}
  eof	   {TEOF $$}

%%


Stmts :: {Stmts}
        : Stmt         {[$1]}
        | Stmts Stmt   {$2:$1}

Stmt :: {Stmt}
        : VAssignment {$1}
        | CAssignment           {$1}
--        | Command      {$1}

VAssignment :: {Stmt}
        : Variable '=' Term {VAssgn $1 $3 }


Variable :: {Variable}
        : var        {Var (fst $1) (snd $1)}

CAssignment :: {Stmt}
        : ContextWord'=' Term {CAssgn $1 $3 }

ContextWord :: {ContextWord}
        : ctxWord {CtxWord (fst $1) (snd $1)}

Term :: {Term}
        : num       {Num (fst $1) (snd $1)}
        | note      {Note (fst $1) (snd $1)}
        | Term '#'  {Sharp $1}
        | '{' CAssignments '}'  {Struct $2}

CAssignments :: {Stmts}
        : CAssignment {[$1]}
        | CAssignments ',' CAssignment  {$3:$1}


--        | chord    {($1,Chord (snd chord))}
--        | '{'  BAssignments  '}' {($1, Struct $2) }
--        | Command           {$1}
--        | String            {$1}
--        | Term '/' Term  {(fst $1, Inverted $1 $2)}

--
--Command :: {Command}
--        : chordK Term   {ChordK1 (fst chordK) $2 }
--        | ChordK1 Term  {ChordK2 $2 ()}
--
--
--t : num {Int (fst $1) (snd $2)}
--
--
--
{
--data Program = Stmts
type Stmts = [Stmt]
data Stmt = VAssgn Variable Term | CAssgn ContextWord Term
        deriving(Show,Eq)
data Variable = Var AlexPosn String
        deriving(Show,Eq)
data ContextWord = CtxWord AlexPosn String
        deriving(Show,Eq)
data Term = Num AlexPosn Integer | Note AlexPosn Char | Sharp Term  
        | Struct Stmts
        deriving(Show,Eq)
parseError _ = error "Parse error"



--data ValueToken = TNum (AlexPosn,Integer) | TNote (AlexPosn,String) | TChord (AlexPosn,String) | TVar (AlexPosn,String)
--type Value = Integer | String

--tokenPos :: Token -> AlexPosn
--tokenPos (TNum (p,_)) -> p
--tokenPos (TNote (p,_)) -> p
--tokenPos (TChord (p,_)) -> p
--tokenPos (TOBracket p) -> p
--tokenPos (TCBracket p) -> p
--tokenPos (TQuant p)  -> p     
--tokenPos (TTempo p)  -> p
--tokenPos (TKey p)-> p
--tokenPos (TOctavePos p) -> p
--tokenPos (TEq p) -> p
--tokenPos (TInputK p) -> p
--tokenPos (TChordK p) -> p
--tokenPos (TQuote p) -> p
--tokenPos (TComma p) -> p
--tokenPos (TDot p) -> p
--tokenPos (TDiv p) -> p
--tokenPos (TPlus p) -> p
--tokenPos (TMinus p) -> p
--tokenPos (TSharp p) -> p
--tokenPos (TMin p) -> p
--tokenPos (TFlat p) -> p
--tokenPos (TUnderscore p) -> p
--tokenPos (TR p) -> p
--tokenPos (TVar (p, _)  -> p
--
--tokenValue :: ValueToken -> Value
--tokenValue TNum (p,v) -> v
--tokenValue TNote (p,s) -> s
--tokenValue TChord (p,s) -> s
--tokenValue TVar (p,s) -> s



--data E a = Ok a | Failed String
--
--thenE :: E a -> (a -> E b) -> E b
--m `thenE` k = 
--   case m of 
--       Ok a -> k a
--	 Failed e -> Failed e
--
--returnE :: a -> E a
--returnE a = Ok a
--
--failE :: String -> E a
--failE err = Failed err
--
--catchE :: E a -> (String -> E a) -> E a
--catchE m k = 
--   case m of
--      Ok a -> OK a
--	Failed e -> k e
}