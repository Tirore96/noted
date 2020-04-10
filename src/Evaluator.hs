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


evaluate = undefined
--evaluate (In comp con) = evaluateComp comp con 1
--
--safeLookup key con = case Map.lookup key con of 
--                      Just n -> Right n
--                      Nothing -> Left "internal error-label dependency not met"
--
--evaluateComp (NoteN note n _) con nominator = do Num q _ <- safeLookup Quantization con
--                                                 Right ((strToNoteFun note) (fromIntegral n) (nominator%q))
--
--evaluateComp (Dotted comp) con nominator = evaluateComp comp con (nominator+1)
--
--evaluateComp (Concatted comp1 comp2) con nominator = do m1 <- evaluateComp comp1 con nominator
--                                                        m2 <- evaluateComp comp2 con nominator
--                                                        Right (m1 :+: m2)

                 

