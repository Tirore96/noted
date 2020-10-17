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
$whitespace+         {skip}
(\n)+                               {mkL TNewLine}
\(                                  {mkL TOPara}
\)                                  {mkL TCPara}
--\<serial\,[0-8]\>                    {mkL TSerial}
--\<parallel\,[0-8]\>                  {mkL TParallel}
=                                   {mkL TEq}
\|+                                  {mkL TParallel}
\-+                                  {mkL TSerial}
\_+                                  {mkL TUnderscore}
0|[1-9]([0-9]*)                       {mkL TNum}
[a-g]                               {mkL TLetter}
\\                                  {mkL TBackslash}
case | endcase | of | play | let | in {mkL TKeyword}

[h-z]|[a-z]($alpha|$digit|\_)+           {mkL TVar}
\->                                  {mkL TArrow}
\,                                   {mkL TComma}
\.                                   {mkL TDot}







{


--makeState s= AlexState {alex_pos = alexStartPos, alex_inp = s,alex_chr = '\n',alex_bytes=[],alex_scd=0}

data TokenClass = 
    TNewLine |
    TOPara |
    TCPara |
    TSerial |
    TParallel |
    TUnderscore |
    TEq | 
    TNum |
    TVar |
    TLetter |
    TBackslash |
    TKeyword |
    TLet |
    TArrow |
    TComma |
    TDot |
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
             
scanner s = do tokens <- runAlex s alexJoin
               return $ (removeTrailingNewLines tokens) ++ [T (AlexPn 0 0 0) TNewLine "" ]
           
removeTrailingNewLines tokens = case (last tokens) of
                                  (T _ TNewLine _) -> removeTrailingNewLines (init tokens)
                                  _ -> tokens
}
