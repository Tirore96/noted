{
module Calc.Lexer(alexScanTokens,Token(..),AlexPosn(..)) where
}

%wrapper "posn"

$digit = 0-9-- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-
  $white+                                        ; 
  "//".*                                         ;
  $digit+                                        {\p s -> TNum  (p, (read s)) }
  (a|[c-h])                                      {\p s -> TNote (p, head s)}
  (A|[C-H])                                      {\p s -> TChord (p,s)}
  \{                                             {\p s -> TOBracket p}
  \}                                             {\p s -> TCBracket p}
  quantization | tempo | key | octave\_pos       {\p s -> TCtxWord (p,s)}
  =                                              {\p s -> TEq p}
  input                                          {\p s -> TInputK p}
  chord                                          {\p s -> TChordK p}
  \"                                             {\p s -> TQuote p}
  \,                                             {\p s -> TComma p}
  \.                                             {\p s -> TDot p}
  \/                                             {\p s -> TDiv p}
  \+                                             {\p s -> TPlus p}
  \-                                             {\p s -> TMinus p}
  \#                                             {\p s -> TSharp p}
  \m                                             {\p s -> TMin p}
  b                                              {\p s -> TFlat p}
  \_                                             {\p s -> TUnderscore p}
  R                                              {\p s -> TR p}
  \$ $alpha ($alpha|$digit|\_)*                  {\p s -> TVar (p, s)}

{

data Token =
    TNum  (AlexPosn, Integer)  |
    TNote (AlexPosn, Char)  | 
    TChord (AlexPosn, String)  |
    TOBracket AlexPosn      |
    TCBracket AlexPosn      |
    TCtxWord (AlexPosn,String)     |
    TEq AlexPosn            |
    TInputK AlexPosn        |
    TChordK AlexPosn        |
    TQuote AlexPosn         |
    TComma AlexPosn         |
    TDot AlexPosn           |
    TDiv AlexPosn           |
    TPlus AlexPosn          |
    TMinus AlexPosn         |
    TSharp AlexPosn         |
    TMin AlexPosn           |
    TFlat AlexPosn          |
    TUnderscore AlexPosn    |
    TR AlexPosn             |
    TVar (AlexPosn, String) |
    TEOF AlexPosn
    deriving (Eq,Show)
}