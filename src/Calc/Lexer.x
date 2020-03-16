{
module Calc.Lexer where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$pitch = \#  | b
$shift = + | -
$note = [A-H]($pitch)?  
$pShiftedNote = $note ($shift)?
$diatonic = 1-7
$pShiftedDiatonic = $diatonic ($shift)?
$numericExtension = 7 | 9 | 11 | 13
$extension = $numericExtension ($pitch)?

$chord = $note (m)? $extension*
$invertedChord = $chord ([0-2])

tokens :-

  \" ($pShiftedDiatonic \,)*  $pShiftedDiatonic \"                   {\s -> TRelInput s}
  \" ($pShiftedNote \, )* $pShiftedNote\"  {\s TAbsInput s}
  \" $chord | $invertedChord | \. | R |   \"
  \{|\}|\;|\#|\"|\,|\.|\(|\)|\-\>        {\s -> TSChar (head s)}
  $white+				;
  "//".*                                 ;
    
  input 				{ \s -> TInput}
  chord                                 { \s -> TChord}
  $digit+				{ \s -> TNum  (read s) }
--  [\=\+\-\*\\\%\==]			{ \s -> TOp (head s) }
  $alpha($alpha|$digit|\_)*             { \s -> TVar s }
  $note                           {\s -> TNote s}
  
{
-- Each right-hand side has type :: String -> Token
-- The token type:
data Token =
 	TSChar Char 	|
	TInput		|
	TChord		|
	TNum Integer    |
	TOp Char	|
	TVar String	|
	TNote String	|
	TEOF
	deriving (Eq,Show)

--main = do
--  name <- getContents
--  s <- readFile name
--  print (alexScanTokens s)
}
