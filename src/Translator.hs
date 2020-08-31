module Translator where

import qualified Euterpea as Eu
import qualified TermReducer as TR
import qualified Calc.Parser as P
import Data.Ratio


translate :: TR.ReducedTerm -> Either String (Eu.Music Eu.Pitch)
translate (TR.Single base) = translateBase base
translate (TR.FlatList (ct,ts)) = do translated_ts <- mapM translate ts
                                     combinePitches ct translated_ts
                                    

combinePitches :: P.CompType -> [Eu.Music Eu.Pitch] -> Either String (Eu.Music Eu.Pitch)
combinePitches _ [] = Left "combinePitches error"
combinePitches _ [pitch] = Right pitch
combinePitches P.Serial (pitch:rest) = do other_pitch <- combinePitches P.Serial rest
                                          return  $ (Eu.:+:) pitch other_pitch
combinePitches P.Parallel (pitch:rest) = do other_pitch <- combinePitches P.Parallel rest
                                            return $  (Eu.:=:) pitch other_pitch



translateBase :: TR.ReducedBase -> Either String (Eu.Music Eu.Pitch)
translateBase (TR.RNote ((index,octave),dur)) =
  let letter_fun = indexToLetterFun index
      final_octave = fromIntegral octave
      final_dur = 1%dur
  in Right $ letter_fun final_octave final_dur

translateBase _ = Left "translation error"



indexToLetterFun :: Integer -> (Eu.Octave -> Eu.Dur -> Eu.Music Eu.Pitch)
indexToLetterFun 0 = Eu.a
indexToLetterFun 1 = Eu.as
indexToLetterFun 2 = Eu.b
indexToLetterFun 3 = Eu.c
indexToLetterFun 4 = Eu.cs
indexToLetterFun 5 = Eu.d
indexToLetterFun 6 = Eu.ds
indexToLetterFun 7 = Eu.e
indexToLetterFun 8 = Eu.f
indexToLetterFun 9 = Eu.fs
indexToLetterFun 10 = Eu.g
indexToLetterFun 11 = Eu.gs
indexToLetterFun n = let safe_n = fromIntegral (n `mod` 12)
                     in indexToLetterFun safe_n








--letterToFun :: (P.Letter,P.Sign) -> (Eu.Octave -> Eu.Dur -> Eu.Music Eu.Pitch)
--letterToFun (P.A,P.Flat) = Eu.af
--letterToFun (P.A,P.Natural) = Eu.a
--letterToFun (P.A,P.Sharp) = Eu.as
--letterToFun (P.B,P.Flat) = Eu.bf
--letterToFun (P.B,P.Natural) = Eu.b
--letterToFun (P.B,P.Sharp) = Eu.bs
--letterToFun (P.C,P.Flat) = Eu.cf
--letterToFun (P.C,P.Natural) = Eu.c
--letterToFun (P.C,P.Sharp) = Eu.cs
--letterToFun (P.D,P.Flat) = Eu.df
--letterToFun (P.D,P.Natural) = Eu.d
--letterToFun (P.D,P.Sharp) = Eu.ds
--letterToFun (P.E,P.Flat) = Eu.ef
--letterToFun (P.E,P.Natural) = Eu.e
--letterToFun (P.E,P.Sharp) = Eu.es
--letterToFun (P.F,P.Flat) = Eu.ff
--letterToFun (P.F,P.Natural) = Eu.f
--letterToFun (P.F,P.Sharp) = Eu.fs
--letterToFun (P.G,P.Flat) = Eu.gf
--letterToFun (P.G,P.Natural) = Eu.g
--letterToFun (P.G,P.Sharp) = Eu.gs
--
