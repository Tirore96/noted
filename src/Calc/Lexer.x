{
--module Calc.Lexer(alexScanTokens,Token(..),AlexPosn(..)) where
module Calc.Lexer where

}

%wrapper "monad"

$digit = 0-9-- digits
$alpha = [a-zA-Z]   -- alphabetic characters
$whitespace = [\ \t\f\v\r] 
@note = a|[c-h]

tokens :-
--general tokens
$whitespace+                                   {skip}     
=                                              {mkL TEq}
\,                                              {mkL TComma}
\|                                              {mkL TParallel}
\-                                              {mkL TSerial}

\$$alpha ($alpha|$digit|\_)* | main                   {mkL TVar}
$digit                                        {mkL TNum}
@note (\#|b)?                             {mkL TLetter}
"//".* \n                                         {skip}

--Outer scope tokens
<0> (\n)+                                           {mkL TNewLine}
<0>  \{                                             {mkL TOBracket `andBegin` struct}
<0> \(                                         {mkL TOPara}
<0> \)                                             {mkL TCPara}
<0> wn | hn | qn | en                              {mkL TDur}
<0> toNotes | toMusic | transform                 {mkL TFun}
<0> \-\>                                {mkL TArrow}
--Struct scope
<struct> key | octave                             {mkL TCtxLabel}
<struct> \;                                        {mkL TSemi}
<struct>  \}                                             {mkL TCBracket `andBegin` 0}
<struct> (\n)                                 {skip} --newlines skipped in struct


{


--makeState s= AlexState {alex_pos = alexStartPos, alex_inp = s,alex_chr = '\n',alex_bytes=[],alex_scd=0}

data TokenClass = 
    TDur|
    TMain|
    TNum   |
    TLetter | 
    TOBracket |
    TCBracket |
    TCtxLabel|
    TEq |
    TSemi|
    TEOF |
    TOPara |
    TCPara |
    TVar |
    TComma |
    TNewLine |
    TParallel|
    TSerial |
    TFun |
    TArrow
  deriving (Eq,Show)
  

data Token = T AlexPosn TokenClass String
  deriving (Eq)

instance Show Token where
  show (T _ c s) = "("++(show c)++" "++s++")"

--Inspired from https://github.com/simonmar/alex/blob/master/examples/haskell.x
mkL :: TokenClass -> AlexInput -> Int -> Alex Token
mkL c (p,_,_,str) len = return (T p c (take len str))

alexEOF = return $ T undefined TEOF ""
		
alexJoin = alexMonadScan >>= \token -> 
			  case token of
	      		      T _ TEOF _ -> return []
	      		      _       -> alexJoin >>= \tokens -> return (token:tokens)

scanner s = runAlex s alexJoin
	      
}
