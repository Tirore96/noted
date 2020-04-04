{
module Calc.Parser where

import Calc.Lexer


}

%monad { Either String } {>>= } {return }

--tree



%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  num  {T _ TNum _}
  note {T _ TNote _}
  chord {T _ TChord _ }
  '{'  {T _ TOBracket _}
  '}'  {T _ TCBracket _}
  ctxWord {T _ TCtxWord _}
  '=' {T _ TEq _ }
  ',' {T _ TComma _}
  '.' {T _ TDot _}
  ';'  {T _ TSemi _ }
--  eof	   {T _ TEOF _}
--  '/' {T _ TDiv _}
--  '+' {T _ TPlus _}
--  '-' {T _ TMinus _}
--  '#' {T _ TSharp _}
--  minor {T _ TMin _}
--  flat {T _ TFlat _}
--  '_' {T _ TUnderscore _}
--  R {T _ TR _ }
--  var {T _ TVar _}

%%


Exp :: {Exp}
        : Composition Context {In $1 $2}

Composition :: {Composition}
	: note num {NoteN $1 $2}
        | Composition '.' {Dotted $1}
	| Composition Composition {Concatted $1 $2}

Context :: {Context}
	: '{' CtxAssignments '}' {reverse $2}

CtxAssignments :: {[(Token,Token)]}
	      : CtxAssignments ';' CtxAssignment {$3:$1}
	      | CtxAssignment {[$1]}

CtxAssignment :: {(Token,Token)}
	: ctxWord '=' CtxVal {($1,$3)}

CtxVal :: {CtxVal}
       : num    {$1}


-- Stmt :: {Stmt}
--         : VAssignment {$1}
--         | CAssignment           {$1}
-- --        | Command      {$1}
-- 
-- VAssignment :: {Stmt}
--         : Variable '=' Term {Assgn $1 $3 }
-- 
-- 
-- Variable :: {Location}
--         : var        {parseVar $1 }
-- 
-- CAssignment :: {Stmt}
--         : ContextWord'=' Term {Assgn $1 $3 }
-- 
-- ContextWord :: {Location}
--         : ctxWord {parseCtx $1}
-- 
-- Term :: {Term}
--         : num       {parseNum $1}
--         | note      {parseNote $1}
--         | Term '#'  {Unary $1 Sharp}
--         | '{' CAssignments '}'  {Struct $ reverse $2}
-- 
-- CAssignments :: {Stmts}
--         : CAssignment {[$1]}
--         | CAssignments ',' CAssignment  {$3:$1}
-- 
-- 
--        | chord    {tokenVal $1}
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
--parseVar (T p TVar s) = Var p s
--parseCtx (T p TCtxWord s) =  CtxWord p s
--parseNum (T p TNum s) =  Num p (read s)
--parseNote (T p TNote s) = Note p s


--------------------------------------------
data Exp = In Composition Context -- | In Composition Context Exp
  deriving(Eq,Show)

--type Pos = (Int,Int)

data Composition = NoteN Token Token | Dotted Composition | Concatted Composition Composition
  deriving(Eq,Show)

type Context = [(Token,Token)]

type CtxLabel = Token

type CtxVal = Token
------------------------------------------------

--type Stmts = [Stmt]
--
--
--data Stmt = Term | Assgn Location Term
--        deriving(Show,Eq)
--
--
--data Location = Var AlexPosn String| CtxWord AlexPosn String
--        deriving(Show,Eq)
--
--type Args = [Term]
--
--data CType = Notes | Seq 
--    deriving (Show,Eq)
--        
--data Term = Num AlexPosn Integer | Note AlexPosn String  | Chord AlexPosn String
--        | Struct Stmts | CommaTs [Term] | DotTs [Term] | Unary Term Op | R | Extended Term Term |
--          Command CType Args
--
--        deriving(Show,Eq)
--
--data Op = Plus | Minus | Sharp | Flat | Underscore
--        deriving(Show,Eq)

parseError tokens = let (T p _ s)=(head tokens)
		    in let (AlexPn _ l c)=p
		       in let msg = "Parse error on line " ++ (show l) ++ ", column "++(show c)++" during parsing of "++s
			  in Left msg
parseError [] = Left "empty token list but failed"

}
