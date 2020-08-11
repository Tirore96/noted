module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Calc
import Control.Monad.State.Lazy
--import LexerTests
--import ParserTests
import Data.Map as Map
import TypeChecker

main = do
  defaultMain (testGroup "All tests" [positiveMusicTest,
                                      negativeCompTestNumShort,
                                      negativeCompTestNumLong,
                                      negativeCompTestLetter,
                                      positiveCompTestToNotes,
                                      negativeCompTestToNotes,
                                      negativeApplication1, --lexerGroup
--                                      negativeDur,
                                      negativeApplication2])



--typeCheckTests = testGroup "Type check tests" [compositionTests,applicationTests]
--
--composistionTests = testGroup "composition tests" [negativeCompTest,positiveCompTest]
--
--applicationTests = testGroup "application tests" [negativeApplicationTest,positiveApplicationTest]

--typeCheckString :: String -> Either (Maybe TypedTerm,String) (Maybe TypedTerm,[Constraint])
typeCheckString str = let m = do tokens <- scanner str
                                 parseTokens tokens
                      in case m of 
                           Right a -> let state = StateData [] 0 Map.empty
                             in let (_,newState) = runState (genConstraints a) state 
                                in let cs = constraints newState
                                       g = gamma newState
                                   in case solve cs of
                                           Right r -> Right (g,r)
                                           Left l -> Left (g,l)

                                      
                                       
                           Left err -> undefined
                --typeCheck ast

eitherToBool (Left _) = False
eitherToBool (Right _) = True

typeCheckTest name str b = 
  testCase name $ do
    case typeCheckString str of
      Left (var,res) -> assertBool ((show res)++" failed with program:\n"++str++"\nvariable:"++(show var)) (False==b)
      Right (var,res) -> assertBool ((show res)++" failed with program:\n"++str++"\nvariable:"++(show var)) (True==b)

                      
positiveMusicTest = let name = "basic music test"
                        str = "$n = toNotes(qn,1) \n$c = {octave=4,key=c} \nmain = toMusic($n,$c)"
                    in typeCheckTest name str True


negativeCompTestNumShort = 
  let name = "(negative) composing TDur with TNum (Short)"
      str = "main = toMusic(toNotes(qn,qn|1),{octave=4,key=c})"
  in typeCheckTest name str False

negativeCompTestNumLong = 
  let name = "(negative) composing TDur with TNum (Long)"
      str = "$d=qn\n$p=qn|1\n$n=toNotes($d,$p)\n$c={octave=4,key=c}\nmain=toMusic($n,$c)"
  in typeCheckTest name str False

negativeCompTestLetter = 
  let name = "(negative) composing TDur with TLetter"
      str = "main = toMusic(toNotes(qn,qn|c),{octave=4,key=c})"
  in typeCheckTest name str False

positiveCompTestToNotes = 
  let name = "(positive) composing TNums and making notes with correct arguments"
      str = "$d = qn-qn\nmain = toMusic(toNotes($d,1|1|1-3|3|3),{octave=4,key=c})"
  in typeCheckTest name str True
  
negativeCompTestToNotes = 
  let name = "(negative) composing TNums with TLetter and making notes (incorrect arguments)"
      str = "main = toMusic(toNotes(qn-qn-qn,1|1|1-3|3|3),{octave=4,key=c})"
  in typeCheckTest name str False

negativeApplication1 = 
  let name = "(negative) application"
      str = "main = toMusic(toNotes(qn-qn-qn,1|1|1-3|3|3),{octave=4})"
  in typeCheckTest name str False

negativeApplication2 = 
  let name = "(negative) application toNotes swapped arguments"
      str = "main = toMusic(toNotes(1|1|1-3|3|3,qn-qn-qn),{octave=4,key=c})"
  in typeCheckTest name str False

negativeDur = 
  let name = "(negative) dur test"
      str = "$d = qn-qn"
  in typeCheckTest name str 

--                    in typeCheckTest str

--                     expected @=? typeCheckString str
--                   where
--                     expected = ?
--                     str = "$c = 1 | qn \n $con = {key=c,octave=4} \n "
                   
                   
