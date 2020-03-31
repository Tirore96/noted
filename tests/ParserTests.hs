module ParserTests where

import Test.Tasty
import Test.Tasty.HUnit
import Calc.Parser
import Definitions







parserGroup = testGroup "Parser tests" [firstTest]


buildVAssgn s t = VAssgn (Var testPos s) t

buildCAssgn s t = CAssgn (buildCtxW s) t


buildCtxW :: String -> ContextWord
buildCtxW s = CtxWord testPos s

buildNum n = Num testPos n


firstTest = testCase "initial test" $
                do testHappy input @=? expected
                where input = "$con1 = {quantization=8,\n tempo=120,key=c#,octave_pos=4"
                      struct = Struct [buildCAssgn "quantization" (buildNum 8),
                                       buildCAssgn "tempo" (buildNum 120),
                                       buildCAssgn "key" (Sharp $ Note testPos 'c')]
                      expected = [buildVAssgn"$con1" struct]
