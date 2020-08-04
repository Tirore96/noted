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
  '->'  {T _ TArrow _}

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
        : Term '(' TermArgs ')' {Application (($1,(reverse $3)),getPos $1)} 
        | Term1 {$1}

TermArgs :: {[Term]}
          : TermArgs ',' Term {$3:$1}
          | Term {[$1]}

Term1 :: {Term}
        : Term2 '->' Term2 {Pattern (($1,$3),(getPos $1))}  -- <--- Remember to add pattern
        | Term2 {$1}

Term2 :: {Term}
        : Term2 '-' Term3 {buildFlatList Serial $1 $3}
        | Term3 {$1}

Term3 :: {Term}
        : Term3 '|' Term4 {buildFlatList Parallel $1 $3}
        | Term4 {$1}

Term4 :: {Term}
        : num {Num (parseBaseInt $1)}
        | dur {Dur (parseDur $1)}
        | note {Letter (parseBase $1)}
        | var {Variable (parseBase $1)}
        | fun {Function (parseFun $1)}
        | '{' ListPairs '}' {parseCon $1 $2}
        | '(' Term ')' {$2}

ListPairs :: {[(Label,Term)]}
	      : ListPairs ',' Pair {$3:$1}
	      | Pair {[$1]}

Pair :: {(Label,Term)}
	: ctxLabel '=' Term4 {(parseCtxLabel $1,$3)}
  
{
sortPairs :: [(Label,Term)] ->  [(Label,Term)]
sortPairs labels = Data.List.sortBy (flip compare `Data.Function.on` fst) labels

parseCon :: Token -> [(Label,Term)] -> Term
parseCon (T pos _ _) pairs = let sorted = sortPairs pairs
                             in Context (sorted,parseAlexPosn pos)

parseBaseInt :: Token -> (Integer,Pos)
parseBaseInt (T pos _ str) = (read str,parseAlexPosn pos)

parseFun :: Token -> (String,Pos)
parseFun (T pos _ s) = (s,parseAlexPosn pos)


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
  deriving(Eq)

instance Show Assignment where
  show (Assignment (s,p) term )= s ++ "=" ++ (show term)

data CompType = Serial | Parallel
  deriving(Eq)

instance Show CompType where
  show Serial = "-"
  show Parallel = "|"



type Pos = (Int,Int)

getPos :: Term -> Pos
getPos (Num (_,p)) = p
getPos (Dur (_,p)) = p
getPos (Letter (_,p)) = p
getPos (FlatList (_,p)) = p
getPos (Pattern (_,p)) = p
getPos (Variable (_,p)) = p
getPos (Context (_,p)) = p
getPos (Application (_,p)) = p
getPos (Function (_,p)) = p









data Term = Num (Integer,Pos)
             | Dur (Integer,Pos)
             | Letter (String,Pos)
             | FlatList ((CompType,[Term],Integer),Pos)
	     | Pattern ((Term,Term),Pos)
             | Variable (String,Pos)
             | Context ([(Label,Term)],Pos)
             | Application ((Term, [Term]),Pos)
             | Function (String,Pos)
--             | ArrowVar (String,Pos)
  deriving(Eq)


buildFlatList :: CompType -> Term -> Term -> Term
buildFlatList compType t1 t2 = 
  let makeFlatListMember flat_notes = case flat_notes of
                                         FlatList ((compType1,l,n),_) -> 
                                              if compType == compType1 
                                                then (l,n)
                                                else ([flat_notes],n)
                                         base -> ([base],1)
  in let (t1_member,n1) = makeFlatListMember t1
         (t2_member,n2) = makeFlatListMember t2
         n = if compType == Serial
               then n1+n2
               else 1
     in FlatList ((compType,(t1_member++t2_member),n),getPos t1)


instance Show Term where
  show (Num (n,_)) = show n
  show (Dur (4,_)) = "qn"
  show (Dur (1,_)) = "hn"
  show (Dur (8,_)) = "en"
  show (Letter (s,_)) = s
  show (FlatList ((t,[last],_),_)) = (show last) 
  show (FlatList ((t,(first:rest),_),_)) = (show first) ++ (show t) ++  (show (FlatList ((t,rest,0),undefined) ))
  show (Variable (s,_)) = s
  show (Context (ctx,_))= show ctx
  show (Application ((t1,terms),_)) = (show t1)++"(" ++ (show terms) ++ ")"
  show (Pattern ((t1,t2),_)) = (show t1) ++ "->" ++ (show t2)
  show (Function (s,_)) = s



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

--data Function = ToNotes | ToMusic | Transform
--  deriving(Eq,Show)





parseError (first:rest)= let (T p _ s)=first
		    in let (AlexPn _ l c)=p
		       in let msg = "Parse error on line " ++ (show l) ++ ", column "++(show c)++" during parsing of "++s
			  in Left msg
parseError [] = Left "empty token list but failed"

}
