module Main where

import Test.Tasty
import Test.Tasty.HUnit
import LexerTests
import ParserTests

main = do
  defaultMain (testGroup "All tests" [lexerGroup,parserGroup])
