module Evaluator where

import Euterpea
import Data.Ratio

evaluate :: Exp -> Music 
evaluate (In comp con) = case comp of 
			  Note Integer -> 

