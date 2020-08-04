module ParserTests where

import Test.Tasty
import Test.Tasty.HUnit
import Evaluator
import Calc.Lexer
import Calc.Parser
import TypeChecker
import qualified Data.Map as Map
import Control.Monad.State.Lazy
import Debug.Trace



parserGroup = testGroup "tests" [flattenTest "main = toNotes(qn - qn ,1|3 - 1)"]


--buildAssgn s t = Assgn (buildVar s) t
--
--buildVar :: String -> Location
--buildVar s = if elem s ["quantization","tempo","key","octave_pos"]
--                  then CtxWord testPos s
--                  else Var testPos s
--
----Terms
--buildNum n = Num testPos n
--buildNums nums = map buildNum nums
--
--buildNote s = Note testPos s
--
--buildChord s = Chord testPos s
--
--buildStruct vars terms = let zipped = zip vars terms
--                         in let assignments = map (\(var,term)->buildAssgn var term) zipped
--                            in Struct assignments
                        

testEvaluation s = let m =  do tokenized <- scanner s
                               ast <- parseTokens tokenized
                               typed <- typeCheck ast
                               evaluate ast
                  in m

strToNotes str =  
  do tokenized <- scanner str
     ast <- parseTokens tokenized
     case evaluateTest ast of
       Right (ENotes n) -> Right n
       Left l -> Left l
  
 
--scan_parse_eval_expected s = 
-- do tokenized <- scanner s
--    ast <- parseTokens tokenized
--    case runStateT (evaluateTerm ast) Map.empty of
--      Right (ENotes n,_) -> Right n
--      Left err -> Left err

encodeDecode (Right notes)= 
                     let flat = flattenComposition notes
                     in Right $ rebuildComposition (traceShow flat flat)
encodeDecode left = left
                  
--instance Eq (Either a b) where
--  (==) (Right l1) (Right l2) = (==) l1 l2
--  (==) val1 val2 = False
--  (/=) val1 val2 = not ((==) val1 val2)

--scan_parse_eval_actual s =                   
--  do evaluated <- scan_parse_eval_expected s
--     case evaluated of 
--       Right (ENotes n)-> Right $ flattenComposition n
--       Left val  -> Left val
       

assertRight (Right val) = assertBool "fine" True
assertRight (Left val) = assertBool "not fine" False

flattenTest s = testCase ("flatten Test with "++ s) $
                let expected = strToNotes s 
                in let actual = encodeDecode expected
                   in do assertRight expected
                         expected @=?  actual


--recordTest = testCase "initial test" $
--                do testHappy input @=? expected
--                where input = "$con1 = {quantization=8,\n tempo=120,key=c#,octave_pos=4}"
--                      expected = return $ [buildAssgn "$con1" 
--                                          (buildStruct ["quantization","tempo","key","octave_pos"]
--                                          [buildNum 8,buildNum 120, Unary (buildNote "c") Sharp,buildNum 4])]
--
--chordTest = testCase "tests: CommaTs, Chord" $ 
--                  do testHappy input @=? expected
--                  where input = "$chrdA = notes \"1,3,5,6\" $con1"
--                        expected = Right $ [buildAssgn "$chrdA" (Command Notes (buildNums [1,3,5,6]))]
--
--dotTest = testCase "chord test, nums and R" $ 
--                  do testHappy input @=? expected
--                  where input = "$chrdA = seq \"1...2..\" $con1"
--                        expected = Right $ [buildAssgn "$chrdA" (Command Seq (buildNums [1,3,5,6]))]
