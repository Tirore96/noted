module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Tokens

main = do
  defaultMain (testGroup "Our Library Tests" [lexingTest])

lexingTest :: TestTree
lexingTest = testCase "Lexing simple program" (do s <- (readFile "examples/program.mi");
						  let tokens = alexScanTokens s
						  assertEqual "" tokens tokens)
