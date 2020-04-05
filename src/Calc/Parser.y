{
module Calc.Parser where
import Calc.Lexer
import qualified Data.Map as Map



}

%monad { Either String } {>>= } {return }

--tree



%name buildAST
%tokentype { Token }
%error { parseError }

%token
  num  {T _ TNum _}
  note {T _ TNote _}
  chord {T _ TChord _ }
  '{'  {T _ TOBracket _}
  '}'  {T _ TCBracket _}
  ctxWord {T _ TCtxLabel _}
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


PExp :: {PExp}
        : PComposition PContext {PIn $1 $2}

PComposition :: {PComposition}
	: note num {PNoteN $1 $2}
        | PComposition '.' {PDotted $1}
	| PComposition PComposition {PConcatted $1 $2}

PContext :: {PContext}
	: '{' CtxAssignments '}' {reverse $2}

CtxAssignments :: {PContext}
	      : CtxAssignments ';' CtxAssignment {$3:$1}
	      | CtxAssignment {[$1]}

CtxAssignment :: {(Token,Token)}
	: ctxWord '=' PCtxVal {($1,$3)}

PCtxVal :: {PCtxVal}
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

--{
--parseVar (T p TVar s) = Var p s
--parseCtx (T p TCtxWord s) =  CtxWord p s
--parseNum (T p TNum s) =  Num p (read s)
--parseNote (T p TNote s) = Note p s

--tokenToCtxLabel :: Token -> Either String CtxLabel
--tokenToCtxLabel (T _ TCtxWord s) = case s of
--				    "quantization -> Right Quantization
--				    _ -> Left "Unidentified context-label"
--tokenToCtxLabel _ = Left "
{

parseTokens  :: [Token] -> Either String Exp 
parseTokens tokens = do pExp <- buildAST tokens
		        return $ parsePExp pExp


parsePExp :: PExp -> Exp
parsePExp (PIn pComp pCon) = In (parsePComp pComp) (parsePCon pCon)

parsePComp :: PComposition -> Composition
parsePComp (PNoteN t1 t2) = let (T p1 _ s1)=t1
                                (T p2 _ s2)=t2
                            in NoteN s1 (read s2) (parseAlexPosn p1)
parsePComp (PDotted pComp) = Dotted (parsePComp pComp)
parsePComp (PConcatted pComp1 pComp2) = Concatted (parsePComp pComp1) (parsePComp pComp2)


parsePCon :: PContext -> Context
parsePCon pCon = let ctxLsCtxVals = map (\(l,v) -> (parsePCtxLabel l,parsePCtxVal v)) pCon
		 in Map.fromList ctxLsCtxVals


parsePCtxLabel :: PCtxLabel -> CtxLabel
parsePCtxLabel (T _ TCtxLabel s) = case s of
                                   "quantization" -> Quantization
                                   _ -> undefined


parsePCtxVal :: PCtxVal -> CtxVal
parsePCtxVal (T p TNum s) = Num (read s) (parseAlexPosn p)
parseAlexPosn :: AlexPosn -> Pos
parseAlexPosn (AlexPn _ line column) = (line,column)


--------------------------------------------
data PExp = PIn PComposition PContext -- | In Composition Context Exp
  deriving(Eq,Show)

data PComposition = PNoteN Token Token | PDotted PComposition | PConcatted PComposition PComposition
  deriving(Eq,Show)

type PContext = [(Token,Token)]

type PCtxLabel = Token

type PCtxVal = Token

------------------------------------------------
data Exp = In Composition Context -- | In Composition Context Exp
  deriving(Eq,Show)


data Composition = NoteN String Integer Pos | Dotted Composition | Concatted Composition Composition
  deriving(Eq,Show)

type Context = Map.Map CtxLabel CtxVal


data CtxLabel = Quantization 
  deriving(Eq,Show,Ord)


data CtxVal = Num Integer Pos
  deriving(Eq,Show)

type Pos = (Int,Int)


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
