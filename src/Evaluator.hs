module Evaluator where

import Euterpea as Eu
import Data.Map as Map
import Data.Ratio
import Calc.Parser


--play music = Eu.playDev 2

strToNoteFun :: String -> Eu.Octave -> Eu.Dur -> Eu.Music Eu.Pitch
strToNoteFun "a" = a
strToNoteFun "c" = c
strToNoteFun "d" = d
strToNoteFun "e" = e
strToNoteFun "f" = f
strToNoteFun "g" = g
strToNoteFun "h" = b



evaluate (In comp con) = case comp of 
			  NoteN note n _ -> case Map.lookup Quantization con of
                                             Just (Num q _)->  let dur = 1%q
                                                                   fun = strToNoteFun note
                                                               in fun (fromIntegral n) dur
                                             Nothing -> undefined
                          _ -> undefined

