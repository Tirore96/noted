module ParserTests where

import Test.Tasty
import Test.Tasty.HUnit
import Calc.Parser
import Definitions







parserGroup = testGroup "Parser tests" [recordTest]


buildAssgn s t = Assgn (buildVar s) t

buildVar :: String -> Location
buildVar s = if elem s ["quantization","tempo","key","octave_pos"]
                  then CtxWord testPos s
                  else Var testPos s

--Terms
buildNum n = Num testPos n
buildNums nums = map buildNum nums

buildNote s = Note testPos s

buildChord s = Chord testPos s

buildStruct vars terms = let zipped = zip vars terms
                         in let assignments = map (\(var,term)->buildAssgn var term) zipped
                            in Struct assignments
                        




recordTest = testCase "initial test" $
                do testHappy input @=? expected
                where input = "$con1 = {quantization=8,\n tempo=120,key=c#,octave_pos=4}"
                      expected = return $ [buildAssgn "$con1" 
                                          (buildStruct ["quantization","tempo","key","octave_pos"]
                                          [buildNum 8,buildNum 120, Unary (buildNote "c") Sharp,buildNum 4])]

chordTest = testCase "tests: CommaTs, Chord" $ 
                  do testHappy input @=? expected
                  where input = "$chrdA = notes \"1,3,5,6\" $con1"
                        expected = Right $ [buildAssgn "$chrdA" (Command Notes (buildNums [1,3,5,6]))]

dotTest = testCase "chord test, nums and R" $ 
                  do testHappy input @=? expected
                  where input = "$chrdA = seq \"1...2..\" $con1"
                        expected = Right $ [buildAssgn "$chrdA" (Command Seq (buildNums [1,3,5,6]))]
