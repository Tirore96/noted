module LexerTests(lexerGroup) where

import Test.Tasty
import Test.Tasty.HUnit
import Calc.Lexer
import Definitions

lexerGroup = testGroup "Lexer tests" [inputTest,noteTest,chordTest,structTest,chordKTest,inputKTest,inputTest,sharpFlatTest,sharpFlatTestL,dotTest,rTest,varTest,commentTest]


numTest = testCase "numbers test" $ do
    let num1 = "1"
    let num2 = "1000"
    [TNum (testPos,1)] @=? testAlex num1
    [TNum (testPos,1000)] @=? testAlex num2

noteTest = testCase "single note tests" $ do
		let note1 = "a"
		let note2 = "h#"
		let note3 = "hb"
		let note4 = "hb_4"

		[TNote (testPos, head note1)] @=? testAlex note1
		[TNote (testPos,'h'),TSharp testPos] @=? testAlex note2
		[TNote (testPos, 'h'),TFlat testPos] @=? testAlex note3
		[TNote (testPos, 'h'),TFlat testPos,TUnderscore testPos, TNum (testPos,4)] @=? testAlex note4

chordTest = testCase "chord tests without struct" $ do
		let chord1 = "C"
		let chord2 = "Dm"
		let chord3 = "Hm7"
		let chord4 = "Hbm#7"
		let chord5 = "Hbm#7b9b11_3"
		let chord6 = "D/g"
		let chord7 = "D7/g_5"
		let chord8 = "Hbm"
		let chord9 = "Hbm#7#7_2"
		[TChord (testPos,chord1)] @=? testAlex chord1
		[TChord (testPos,"D"), TMin testPos] @=? testAlex chord2
		[TChord (testPos,"H"),TMin testPos,TNum (testPos,7)] @=? testAlex chord3
		[TChord (testPos,"H"),TFlat testPos,TMin testPos,TSharp testPos,TNum (testPos,7)] @=? testAlex chord4
		[TChord (testPos,"H"), TFlat testPos, TMin testPos, TSharp testPos, TNum (testPos,7), TFlat testPos, TNum (testPos,9),TFlat testPos, TNum (testPos,11),TUnderscore testPos,TNum (testPos,3)] @=? testAlex chord5
		[TChord (testPos,"D"),TDiv testPos,TNote (testPos,'g')] @=? testAlex chord6
		[TChord (testPos,"D"),TNum (testPos,7),TDiv testPos,TNote (testPos,'g'),TUnderscore testPos,TNum (testPos, 5)] @=? testAlex chord7
		[TChord (testPos,"H"),TFlat testPos,TMin testPos] @=? testAlex chord8
		[TChord (testPos,"H"),TFlat testPos,TMin testPos,TSharp testPos,TNum (testPos,7),TSharp testPos, TNum (testPos,7),TUnderscore testPos,TNum (testPos,2)] @=? testAlex chord9


structTest = testCase "struct tests" $ do
		let struct1 = "{quantization=16,tempo=120}"
		let struct2 = "{octave_pos=4}"
		[TOBracket testPos,TCtxWord (testPos,"quantization"),TEq testPos,TNum (testPos,16), TComma testPos,TCtxWord (testPos,"tempo"),TEq testPos, TNum (testPos,120),TCBracket testPos] @=? testAlex struct1
		[TOBracket testPos,TCtxWord (testPos,"octave_pos"),TEq testPos,TNum (testPos,4),TCBracket testPos] @=? testAlex struct2

chordKTest = testCase "chord keyword test" $ do
		let keyword = "chord"
		[TChordK testPos] @=? testAlex keyword

inputKTest = testCase "input keyword test" $ do
		let keyword = "input"
		[TInputK testPos] @=? testAlex keyword

inputTest= testCase "Relative, Noted and Positioned chord. Also testing quote, comma, div, plus, minus and underscore" $  do
        let input1 = "\"1+,3-,5,6\""
        let input2 = "\"c,e,g\""
        let input3 = "\"c#_4,e_4,g#_4\""
        [TQuote testPos,TNum (testPos,1),TPlus testPos,TComma testPos,TNum (testPos,3),TMinus testPos,TComma testPos,TNum (testPos,5),TComma testPos,TNum (testPos,6),TQuote testPos]@=? testAlex input1
        [TQuote testPos,TNote (testPos,'c'),TComma testPos,TNote (testPos,'e'),TComma testPos,TNote (testPos,'g'),TQuote testPos]@=? testAlex input2
        [TQuote testPos,TNote (testPos,'c'),TSharp testPos,TUnderscore testPos,TNum (testPos,4),TComma testPos,TNote (testPos,'e'),TUnderscore testPos,TNum (testPos,4),TComma testPos,TNote (testPos,'g'),TSharp testPos,TUnderscore testPos,TNum (testPos,4),TQuote testPos]@=? testAlex input3

sharpFlatTest = testCase "Testing sharps and flats" $ do
        let sharp = "1#"
        let flat = "7b"
        [TNum (testPos,1),TSharp testPos] @=? testAlex sharp
        [TNum (testPos,7),TFlat testPos] @=? testAlex flat

sharpFlatTestL = testCase "Testing sharps and flats occuring on left side of integer" $ do
        let sharp = "#7"
        let flat = "b9"
        [TSharp testPos,TNum (testPos,7)] @=? testAlex sharp
        [TFlat testPos, TNum (testPos,9)] @=? testAlex flat

dotTest = testCase "Testing dot" $ do
        let dot = "\"1...\""
        [TQuote testPos, TNum (testPos,1),TDot testPos,TDot testPos,TDot testPos,TQuote testPos] @=? testAlex dot

rTest = testCase "Testing R" $ do
		let dot = "\"1.R.\""
		[TQuote testPos, TNum (testPos,1), TDot testPos,TR testPos,TDot testPos,TQuote testPos] @=? testAlex dot

commentTest = testCase "comments tests" $ do
	      let comment1 = "//something"
	      let comment2 = "////"
	      [] @=? testAlex comment1
	      [] @=? testAlex comment2

varTest= testCase "variable tests" $ do
		let var1 = "$con"
		let var2 = "$Upper"
		let var3 = "$Upper123"
		let var4 = "$Upper123_something"
		let var5 = "$Cm7"
		[TVar (testPos,var1)] @=? testAlex var1
		[TVar (testPos,var2)] @=? testAlex var2
		[TVar (testPos,var3)] @=? testAlex var3
		[TVar (testPos,var4)] @=? testAlex var4
		[TVar (testPos,var5)] @=? testAlex var5