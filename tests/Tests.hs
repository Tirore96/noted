module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Calc.Lexer

data TestToken =
  TTNum Integer | TTNote String | TTChord String | TTOBracket | TTCBracket | TTQuant | TTTempo | TTKey | TTOctavePos | TTEq | TTInputK | TTChordK | TTQuote | TTComma | TTDot | TTDiv | TTPlus | TTMinus | TTSharp | TTMin | TTFlat | TTUnderscore | TTR | TTVar String
  deriving (Eq,Show)

main = do
  defaultMain (testGroup "chordTest" [inputTest,noteTest,chordTest,structTest,chordKTest,inputKTest,inputTest,sharpFlatTest,sharpFlatTestL,dotTest,rTest,varTest,commentTest])

makeTestable :: [Token] -> [TestToken]
makeTestable [] = []
makeTestable (token:tokens)= case token of
		    	    TNum p n-> TTNum n:  makeTestable tokens
		    	    TNote p s-> TTNote s:  makeTestable tokens
		    	    TChord p s-> TTChord s:  makeTestable tokens
		    	    TOBracket p-> TTOBracket:  makeTestable tokens
		    	    TCBracket p-> TTCBracket:  makeTestable tokens
		    	    TQuant p -> TTQuant:  makeTestable tokens
			    TTempo p -> TTTempo: makeTestable tokens
			    TKey p-> TTKey:  makeTestable tokens
			    TOctavePos p-> TTOctavePos:  makeTestable tokens
			    TEq p-> TTEq:  makeTestable tokens
			    TInputK p-> TTInputK:  makeTestable tokens
			    TChordK p-> TTChordK:  makeTestable tokens
			    TQuote p-> TTQuote:  makeTestable tokens
			    TComma p-> TTComma:  makeTestable tokens
			    TDot p-> TTDot:  makeTestable tokens
			    TDiv p-> TTDiv:  makeTestable tokens
			    TPlus p-> TTPlus:  makeTestable tokens
			    TMinus p-> TTMinus:  makeTestable tokens
			    TSharp p-> TTSharp:  makeTestable tokens
			    TMin p-> TTMin:  makeTestable tokens
			    TFlat p-> TTFlat:  makeTestable tokens
			    TUnderscore p-> TTUnderscore:  makeTestable tokens
			    TVar p s-> TTVar s:  makeTestable tokens
			    TR p-> TTR:  makeTestable tokens



testAlex = makeTestable . alexScanTokens 


numTest = testCase "numbers test" $ do
		let num1 = "1"
		let num2 = "1000"
		[TTNum 1] @=? testAlex num1
		[TTNum 1000] @=? testAlex num2

noteTest = testCase "single note tests" $ do
		let note1 = "a"
		let note2 = "h#"
		let note3 = "hb"
		let note4 = "hb_4"

		[TTNote note1] @=? testAlex note1
		[TTNote "h",TTSharp] @=? testAlex note2
		[TTNote "h",TTFlat] @=? testAlex note3
		[TTNote "h",TTFlat,TTUnderscore, TTNum 4] @=? testAlex note4

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
		[TTChord chord1] @=? testAlex chord1
		[TTChord "D", TTMin] @=? testAlex chord2
		[TTChord "H",TTMin,TTNum 7] @=? testAlex chord3
		[TTChord "H",TTFlat,TTMin,TTSharp,TTNum 7] @=? testAlex chord4
		[TTChord "H", TTFlat, TTMin, TTSharp, TTNum 7, TTFlat, TTNum 9,TTFlat, TTNum 11,TTUnderscore,TTNum 3] @=? testAlex chord5
		[TTChord "D",TTDiv,TTNote "g"] @=? testAlex chord6
		[TTChord "D",TTNum 7,TTDiv,TTNote "g",TTUnderscore,TTNum 5] @=? testAlex chord7
		[TTChord "H",TTFlat,TTMin] @=? testAlex chord8
		[TTChord "H",TTFlat,TTMin,TTSharp,TTNum 7,TTSharp, TTNum 7,TTUnderscore,TTNum 2] @=? testAlex chord9


structTest = testCase "struct tests" $ do
		let struct1 = "{quantization=16,tempo=120}"
		let struct2 = "{octave_pos=4}"
		[TTOBracket,TTQuant,TTEq,TTNum 16, TTComma,TTTempo,TTEq, TTNum 120,TTCBracket] @=? testAlex struct1
		[TTOBracket,TTOctavePos,TTEq,TTNum 4,TTCBracket] @=? testAlex struct2

chordKTest = testCase "chord keyword test" $ do
		let keyword = "chord"
		[TTChordK] @=? testAlex keyword

inputKTest = testCase "input keyword test" $ do
		let keyword = "input"
		[TTInputK] @=? testAlex keyword

inputTest= testCase "Relative, Noted and Positioned chord. Also testing quote, comma, div, plus, minus and underscore" $  do
	        let input1 = "\"1+,3-,5,6\""
		let input2 = "\"c,e,g\""
		let input3 = "\"c#_4,e_4,g#_4\""
		[TTQuote,TTNum 1,TTPlus,TTComma,TTNum 3,TTMinus,TTComma,TTNum 5,TTComma,TTNum 6,TTQuote]@=? testAlex input1
		[TTQuote,TTNote "c",TTComma,TTNote "e",TTComma,TTNote "g",TTQuote]@=? testAlex input2
		[TTQuote,TTNote "c",TTSharp,TTUnderscore,TTNum 4,TTComma,TTNote "e",TTUnderscore,TTNum 4,TTComma,TTNote "g",TTSharp,TTUnderscore,TTNum 4,TTQuote]@=? testAlex input3



sharpFlatTest = testCase "Testing sharps and flats" $ do
		let sharp = "1#"
		let flat = "7b"
		[TTNum 1,TTSharp] @=? testAlex sharp
		[TTNum 7,TTFlat] @=? testAlex flat

sharpFlatTestL = testCase "Testing sharps and flats occuring on left side of integer" $ do
		let sharp = "#7"
		let flat = "b9"
		[TTSharp,TTNum 7] @=? testAlex sharp
		[TTFlat, TTNum 9] @=? testAlex flat


dotTest = testCase "Testing dot" $ do
		let dot = "\"1...\""
		[TTQuote, TTNum 1,TTDot,TTDot,TTDot,TTQuote] @=? testAlex dot

rTest = testCase "Testing R" $ do
		let dot = "\"1.R.\""
		[TTQuote, TTNum 1, TTDot,TTR,TTDot,TTQuote] @=? testAlex dot



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
		[TTVar var1] @=? testAlex var1
		[TTVar var2] @=? testAlex var2
		[TTVar var3] @=? testAlex var3
		[TTVar var4] @=? testAlex var4
		[TTVar var5] @=? testAlex var5












--structTest = testCase "Structs" $ do
--		let input1 = "


