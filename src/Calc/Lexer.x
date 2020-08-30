{
--module Calc.Lexer(alexScanTokens,Token(..),AlexPosn(..)) where
module Calc.Lexer where

}

%wrapper "monad"

$digit = 0-9-- digits
$alpha= [a-zA-Z]   -- alphabetic characters
$whitespace = [\ \t\f\v\r] 
@letter = a|[c-h]

tokens :-
--general tokens
$whitespace+                        {skip}     
"//".*                              {skip}
(\n)+                               {mkL TNewLine}
\(                                  {mkL TOPara}
\)                                  {mkL TCPara}
=                                   {mkL TEq}
\|                                  {mkL TParallel}
\-                                  {mkL TSerial}
wn | hn | qn | en                   {mkL TDur}
o[1-8]                              {mkL TOctave}
[A-G]                               {mkL TLetter}
Major | Minor                       {mkL TColor}
1st | 2nd | 3rd | [4-9] th        {mkL TIndex}
withDur                             {mkL TWithDur}
withOctave                          {mkL TWithOctave}
withScale                           {mkL TWithScale}
withColor                           {mkL TWithColor}
[a-z] ($alpha|$digit|\_)*           {mkL TVar}
{


--makeState s= AlexState {alex_pos = alexStartPos, alex_inp = s,alex_chr = '\n',alex_bytes=[],alex_scd=0}

data TokenClass = 
    TNewLine |
    TOPara |
    TCPara |
    TEq | 
    TVar |
    TParallel |
    TSerial |
    TDur |
    TOctave |
    TLetter |
    TColor |
    TIndex |
    TWithDur |
    TWithOctave |
    TWithScale |
    TWithColor |
    TEOF
  deriving (Eq,Show)
  

data Token = T AlexPosn TokenClass String
  deriving (Eq)

instance Show Token where
  show (T _ c s) = "("++(show c)++" "++s++")"

--Inspired from https://github.com/simonmar/alex/blob/master/examples/haskell.x
mkL :: TokenClass -> AlexInput -> Int -> Alex Token
mkL c (p,_,_,str) len = return (T p c (take len str))

alexEOF = return $ T undefined TEOF ""

alexJoin = do myToken <- alexMonadScan
              case myToken of
                T _ TEOF _ -> return []
                _       -> do tokens <- alexJoin
                              return $ myToken:tokens
             
scanner s = runAlex s alexJoin
}
