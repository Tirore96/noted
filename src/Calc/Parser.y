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
   '|' {Lex.T _ Lex.TParallel _}
   '-' {Lex.T _ Lex.TSerial _}
   '_' {Lex.T _ Lex.TUnderscore _}
--   '<' {Lex.T _ Lex.TLangle _}
--   '>'  {Lex.T _ Lex.TRangle _}
   '='  {Lex.T _ Lex.TEq _}
   n {Lex.T _ Lex.TNum _}
   var {Lex.T _ Lex.TVar _}
   l {Lex.T _ Lex.TLetter _}
   '\\' {Lex.T _ Lex.TBackslash _ }
   case {Lex.T _ Lex.TKeyword "case"}
   endcase {Lex.T _ Lex.TKeyword "endcase"}

   of {Lex.T _ Lex.TKeyword "of"}
--   serial {Lex.T _ Lex.TKeyword "serial"}
--   parallel {Lex.T _ Lex.TKeyword "parallel"}
   fix {Lex.T _ Lex.TKeyword "fix"}
   play {Lex.T _ Lex.TKeyword "play"}
   let {Lex.T _ Lex.TKeyword "let"}
   in {Lex.T _ Lex.TKeyword "in"}
   '->' {Lex.T _ Lex.TArrow _}
   '<->' {Lex.T _ Lex.TPComp "serial"}
   '<|>' {Lex.T _ Lex.TPComp "parallel"}


   ',' {Lex.T _ Lex.TComma _}
   '.' {Lex.T _ Lex.TDot _}

--   eof {Lex.T _ Lex.TEOF _}

%left '-'
%left '_'
%left '|'

%%
Program :: {[Assignment]}
        : Assignments {reverse $1}

Assignments :: {[Assignment]}
        : Assignments Assignment {$2:$1}
        | Assignment {[$1]}

Assignment :: {Assignment}
        : var '=' Exp newline {Assignment (getString $1) $3}

Exp :: {Exp}
          : n  {parse $1}
          | l  {parse $1}
          | '(' Exp ',' Exp ')'  {Tuple (($2,$4),getPos $1) }
          | '.' {parse $1}
          | Exp '-' Exp  {parseComp $1 $2 $3 }
          | Exp '_' Exp  {parseComp $1 $2 $3 }
          | Exp '|' Exp  {parseComp $1 $2 $3 }
          | Exp '<->' Exp  {parseComp $1 $2 $3 }
          | Exp '<|>' Exp  {parseComp $1 $2 $3 }

          | var {parse $1}
          | let var '=' Exp newline in Exp {Let ((getString $2, $4, $7), getPos $1)}
          | play Exp Exp {Play (($2,$3),getPos $1)}
          | '\\' Args '->' Exp   {Lam (($2, $4),getPos $1)}
          | Exp Exp2       {App (($1, $2),getExpPos $1)}
          | case Exp of newline OrderedCaseLines endcase {Case (($2,$5),getPos $1)}
          | '(' Exp ')' {$2}

Exp2 :: {Exp}
          : n  {parse $1}
          | l  {parse $1}
          | '(' Exp2 ',' Exp2 ')'  {Tuple (($2,$4),getPos $1) }
          | '.' {parse $1}
          | Exp2 '-' Exp2  {parseComp $1 $2 $3 }
          | Exp2 '_' Exp2  {parseComp $1 $2 $3 }
          | Exp2 '|' Exp2  {parseComp $1 $2 $3 }
          | var {parse $1}
          | let var '=' Exp newline in Exp {Let ((getString $2, $4, $7), getPos $1)}
          | play Exp2 Exp2 {Play (($2,$3),getPos $1)}
          | '\\' Args '->' Exp2  {Lam (($2, $4),getPos $1)}
          | case Exp of newline OrderedCaseLines endcase {Case (($2,$5),getPos $1)}
          | '(' Exp ')' {$2}


OrderedCaseLines :: {[Exp]}
         : CaseLines {reverse $1}

CaseLines :: {[Exp]}
         : CaseLines CaseLine {$2:$1}
         | CaseLine {[$1]}

CaseLine :: {Exp}
         : Pattern '->' Exp newline {Lam (([$1],$3),getExpPos $1)}

Args :: {[Exp]}
         : Patterns {reverse $1}

Patterns :: {[Exp]}
         : Patterns Pattern {$2:$1}
         | Pattern {[$1]}

Pattern :: {Exp}
          : n  {parse $1}
          | l  {parse $1}
          | '(' Pattern ',' Pattern ')'  {Tuple (($2,$4),getPos $1) }
          | '.' {parse $1}
          | Pattern '-' Pattern {parseComp $1 $2 $3 }
          | Pattern '_' Pattern {parseComp $1 $2 $3 }
          | Pattern '|' Pattern {parseComp $1 $2 $3 }
          | var {parse $1}

{



type Pos = (Int,Int)

data Assignment = Assignment String Exp 
  deriving(Eq,Show)

data Dim = Serial | Parallel
  deriving(Eq,Show)

data Exp =  Num (Integer,Pos)
             | Letter (String,Pos)
             | Tuple ((Exp,Exp),Pos)
             | Dot Pos
             | Cons ((Dim,Int,Exp,Exp),Pos)
             | Var (String,Pos)
             | Let ((String ,Exp ,Exp),Pos)
             | Play ((Exp,Exp),Pos)
             | Lam (([Exp], Exp),Pos)
             | App ((Exp, Exp),Pos)
             | Case ((Exp, [Exp]),Pos)
             | PComp ((Dim,Exp,Exp),Pos)

  deriving(Eq)

instance Show Exp where
  show (Num (n,_)) = show n
  show (Letter (s,_)) = s
  show (Tuple ((e1,e2),_)) = "("++ (show e1) ++ ","++ (show e2) ++ ")"
  show (Dot _) = " . "
  show (Cons ((d,n,e1,e2),_)) = "(" ++ (show e1) ++ "<" ++ (show d)++","++(show n) ++">" ++ (show e2) ++ ")"
  show (Var (s,_)) = s
  show (Let ((s,e1,e2),_)) = "let "++s++" = "++(show e1)++ " in " ++ (show e2)
  show (Play ((e1,e2),_)) = "play " ++ (show e1)++ " "++ (show e2)
  show (Lam ((e1, e2),_)) = "\\"++(show e1)++" "++(show e2)
  show (App ((e1, e2),_)) = (show e1)++" "++(show e2)
  show (Case ((e1, es),_)) = "case "++(show e1)++" of "++(show es)


parseComp :: Exp -> Lex.Token -> Exp -> Exp
parseComp e1 (Lex.T p Lex.TPComp "<->") e2 = PComp ((Serial, e1, e2),parseAlexPosn p)
parseComp e1 (Lex.T p Lex.TPComp "<|>") e2 = PComp ((Parallel, e1, e2),parseAlexPosn p)

parseComp e1 (Lex.T p _ str) e2 = let n = length str 
                                  in let (dim,o) = case (head str) of
                                                     '-' -> (Serial,n-1)
                                                     '_' -> (Serial,0-n)
                                                     '|' -> (Parallel,n-1)
                                     in Cons ((dim,o,e1,e2),parseAlexPosn p)

parseOp :: String -> (Dim,Int)
parseOp str = let str_stripped = init (tail str)
              in if (take 6 str_stripped) == "serial"
                   then (Serial,read (drop 7 str_stripped))
                   else (Parallel,read (drop 9 str_stripped))

                        
--Cons ((Serial, (read $ getString $5), $1, $7),getExpPos $1)
getPos :: Lex.Token -> Pos
getPos (Lex.T p _ _) = parseAlexPosn p

getExpPos :: Exp -> Pos
getExpPos (Num (_,p)) = p
getExpPos (Letter (_,p)) = p
getExpPos (Tuple (_,p)) = p
getExpPos (Dot p) = p
getExpPos (Cons (_,p)) = p
getExpPos (Var (_,p)) = p
getExpPos (Let (_,p)) = p
getExpPos (Play (_,p)) = p
getExpPos (Lam (_,p)) = p
getExpPos (App (_,p)) = p
getExpPos (Case (_,p)) = p

--setExpPos :: Pos -> Exp -> Exp
--setExpPos p1 (Num (v,_)) = Num (v,p1)
--setExpPos p1 (Letter (v,_)) = Letter (v,p1)
--setExpPos p1 (Tuple (v,_)) = Tuple (v,p1)
--setExpPos p1 (Dot _) = Dot p1
--setExpPos p1 (Cons (v,_)) = Cons (v,p1)
--setExpPos p1 (Var (v,_)) = Var (v,p1)
--setExpPos p1 (Let (v,_)) = Let (v,p1)
--setExpPos p1 (Play (v,_)) = Play (v,p1)
--setExpPos p1 (Lam (v,_)) = Lam (v,p1)
--setExpPos p1 (App (v,_)) = App (v,p1)
--setExpPos p1 (Case (v,_)) = Case (v,p1)




getString :: Lex.Token -> String
getString (Lex.T _ _ str) = str

parseAlexPosn :: Lex.AlexPosn -> Pos
parseAlexPosn (Lex.AlexPn _ line column) = (line,column)

parse :: Lex.Token -> Exp
parse (Lex.T p Lex.TNum str) = Num ((read str),parseAlexPosn p)
parse (Lex.T p Lex.TLetter str) = Letter (str,parseAlexPosn p)
parse (Lex.T p Lex.TDot _) = Dot (parseAlexPosn p)
parse (Lex.T p Lex.TVar str ) = Var (str,parseAlexPosn p)






--type Program = [Assignment]
--data Assignment = Assignment (String,Pos) Term
--  deriving(Eq)
--
--instance Show Assignment where
--  show (Assignment (s,p) term )= s ++ "=" ++ (show term)
--


---show





parseError (first:rest)= let (Lex.T p _ s)=first
		    in let (Lex.AlexPn _ l c)=p
		       in let msg = "Parse error on line " ++ (show l) ++ ", column "++(show c)++" during parsing of "++s
			  in Left msg
parseError [] = Left "empty token list but failed"

}
