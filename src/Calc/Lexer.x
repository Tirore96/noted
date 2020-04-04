{
--module Calc.Lexer(alexScanTokens,Token(..),AlexPosn(..)) where
module Calc.Lexer where

}

%wrapper "monad"

$digit = 0-9-- digits
$alpha = [a-zA-Z]   -- alphabetic characters
$whitespace = [\ \t\f\v\r] 

--tokens :-
--  $white+                                        ; 
--  "//".*                                         ;
--  $digit+                                        {\p s -> TNum  (p, (read s)) }
--  (a|[c-h])                                      {\p s -> TNote (p, head s)}
--  (A|[C-H])                                      {\p s -> TChord (p,s)}
--  \{                                             {\p s -> TOBracket p}
--  \}                                             {\p s -> TCBracket p}
--  quantization | tempo | key | octave\_pos       {\p s -> TCtxWord (p,s)}
--  =                                              {\p s -> TEq p}
--  input                                          {\p s -> TInputK p}
--  chord                                          {\p s -> TChordK p}
--  \"                                             {\p s -> TQuote p}
--  \,                                             {\p s -> TComma p}
--  \.                                             {\p s -> TDot p}
--  \/                                             {\p s -> TDiv p}
--  \+                                             {\p s -> TPlus p}
--  \-                                             {\p s -> TMinus p}
--  \#                                             {\p s -> TSharp p}
--  \m                                             {\p s -> TMin p}
--  b                                              {\p s -> TFlat p}
--  \_                                             {\p s -> TUnderscore p}
--  R                                              {\p s -> TR p}
--  \$ $alpha ($alpha|$digit|\_)*                  {\p s -> TVar (p, s)}

tokens :-
  $whitespace+                                        {skip}
  "//".*                                         {skip}
  $digit+                                        {mkL TNum}
  (a|[c-h])                                      {mkL TNote}
  (A|[C-H])                                      {mkL TChord}
  \{                                             {mkL TOBracket}
  \}                                             {mkL TCBracket}
  quantization                                   {mkL TCtxWord}
  =                                              {mkL TEq}
  \;                                              {mkL TSemi}
--  input                                          {mkL TSeq}
--  chord                                          {mkL TNotes}
--  \"                                             {mkL TQuote}
  \,                                             {mkL TComma}
  \.                                             {mkL TDot}
--  \/                                             {mkL TDiv}
--  \+                                             {mkL TPlus}
--  \-                                             {mkL TMinus}
--  \#                                             {mkL TSharp}
--  \m                                             {mkL TMin}
--  b                                              {mkL TFlat}
--  \_ $digit                                      {mkL TUnderscore}
--  R                                              {mkL TR}
--  \$ $alpha ($alpha|$digit|\_)*                  {mkL TVar}


{


--makeState s= AlexState {alex_pos = alexStartPos, alex_inp = s,alex_chr = '\n',alex_bytes=[],alex_scd=0}

data TokenClass = 
    TNum   |
    TNote  | 
    TChord |
    TOBracket |
    TCBracket |
    TCtxWord      |
    TEq |
    TSemi|
    TEOF |
    TComma |
    TDot 

--    TSeq |
--    TNotes |
--    TQuote |
--    TDiv |
--    TPlus |
--    TMinus |
--    TSharp |
--    TMin |
--    TFlat |
--    TUnderscore |
--    TR |
--    TVar  |
    deriving (Eq,Show)

data Token = T AlexPosn TokenClass String
  deriving (Eq,Show)

--Inspired from https://github.com/simonmar/alex/blob/master/examples/haskell.x
mkL :: TokenClass -> AlexInput -> Int -> Alex Token
mkL c (p,_,_,str) len = return (T p c (take len str))

--num :: AlexInput -> Int -> Alex Token
--num (p,_,_,s) len = return (TNum (p,read $ take len s))
--note (p,_,_,s) len = return (TNote (p,take len s))
--chord (p,_,_,s) len = return (TChord (p,take len s))
--TI (p,_,_,s) len = return (TChord (p,take len s)






alexEOF = return $ T undefined TEOF ""
		
alexJoin = alexMonadScan >>= \token -> 
			  case token of
	      		      T _ TEOF _ -> return []
	      		      _       -> alexJoin >>= \tokens -> return (token:tokens)

scanner s = runAlex s alexJoin
	      
}
