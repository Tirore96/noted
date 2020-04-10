{
--module Calc.Lexer(alexScanTokens,Token(..),AlexPosn(..)) where
module Calc.Lexer where

}

%wrapper "monad"

$digit = 0-9-- digits
$alpha = [a-zA-Z]   -- alphabetic characters
$whitespace = [\ \t\f\v\r] 
@note = a|[c-h]

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
<0>  $white+                                        {skip} -- change to whitespace later
<0>  "//".*                                         {skip}
<0>  $digit+                                        {mkL TNum}
<0>  (a|[c-h])                                      {mkL TNote}
<0>  (A|[C-H])                                      {mkL TChord}
<0>  \{                                             {mkL TOBracket `andBegin` struct}
<struct>  \}                                             {mkL TCBracket `andBegin` 0}
<0>  \(                                             {mkL TOPara}
<0>  \)                                             {mkL TCPara}
   =                                              {mkL TEq}
  \;                                              {mkL TSemi}
<0> \$$alpha ($alpha|$digit|\_)*                   {mkL TVar}
<struct> bars | key | time |octave\_pos                             {mkL TCtxWord}
<struct> $digit+                                     {mkL TCtxNum}
<struct> @note (\#|b)?                             {mkL TCtxNote}
<struct> \/                              {mkL TCtxSlash}

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


{


--makeState s= AlexState {alex_pos = alexStartPos, alex_inp = s,alex_chr = '\n',alex_bytes=[],alex_scd=0}

data TokenClass = 
    TNum   |
    TNote  | 
    TChord |
    TOBracket |
    TCBracket |
    TCtxWord |
    TCtxNum|
    TCtxNote|
    TCtxSig |
    TCtxSlash|

    TEq |
    TSemi|
    TEOF |
    TComma |
    TDot |
    TOPara |
    TCPara |
    TVar

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
