{
module Calc.Lexer where
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$shift = [\+\-]
--$note = [A-H]  $pitch  
--$lowercaseNote = [a-h]($pitch)?
--$pShiftedNote = $note ($shift)?
$diatonic = 1-7
--$pShiftedDiatonic = $diatonic ($shift)?
$numericExtension = [7 \ 9 \ 11 \ 13]
--$extension = $numericExtension ($pitch)?

--$chord = $note (m)? $extension* (\/$lowercaseNote)? (\_[1-8])?
--$invertedChord = $chord ([0-2])
--$variable = $alpha($alpha|$digit|\_)*
--  \" ( $diatonic $shift? \,)*  $diatonic $shift? \"  {\p s -> TRelInput p s}
--
----  \" ($alpha  $pitch? ($shift)? \, )* A-H  $pitch? ($shift)? \"	    {\p s  -> TNotedInput p s}
--  \" A-H  $pitch (m)? $numericExtension ($pitch)? (\/[a-h]($pitch)?)? (\_[1-8])? | $alpha($alpha|$digit|\_)* | \. | R |   \"		    {\p s -> TPositionedInput p s}
--  \{|\}|\;					  {\p s -> TSChar p (head s)}

--  \{|\}|\;|\#|\"|\,|\.|\(|\)|\-\>		  {\p s -> TSChar p (head s)}
-- \;                                               {\p s -> TSemiColon p}
--(?!([A-H] (\#|b)? m? ((\#|b)? (7|9|11|13))* (\/ [a-h])? (\_ [1-8])?)|[a-h]  (\#|b)? (\_ [1-8])?) $alpha($alpha|$digit|\_)*    { \p s -> TVar p s }
--  [\=\+\-\*\\\%\==]			{ \s -> TOp (head s) }
--  $alpha($alpha|$digit|\_)*					 { \p s -> TVar p s }


tokens :-
  $white+					                               ; 
  "//".*					                               ;
  $digit+					                               {\p s -> TNum  p (read s) }
  (a|[c-h])  				                         {\p s -> TNote p s}
  (A|[C-H])  	       {\p s -> TChord p s}
  \{                                             {\p s -> TOBracket p}
  \}                                             {\p s -> TCBracket p}
  quantization					                         {\p s -> TQuant p}
  tempo						                               {\p s -> TTempo p}
  key						                                 {\p s -> TKey p}
  octave\_pos					                           {\p s -> TOctavePos p}
  =                                              {\p s -> TEq p}
  input						                               {\p s -> TInputK p}
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
  \$ $alpha ($alpha|$digit|\_)*                                      {\p s -> TVar p s}



  
{
-- Each right-hand side has type :: String -> Token
-- The token type:
data Token =
    TNum  AlexPosn Integer  |
    TNote AlexPosn String   | 
    TChord AlexPosn String  |
    TOBracket AlexPosn      |
    TCBracket AlexPosn      |
    TQuant AlexPosn         |
    TTempo AlexPosn         |
    TKey AlexPosn           |
    TOctavePos AlexPosn     |
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
    TMin AlexPosn	    |
    TFlat AlexPosn          |
    TUnderscore AlexPosn    |
    TR AlexPosn             |
    TVar AlexPosn String      
    deriving (Eq,Show)
}
