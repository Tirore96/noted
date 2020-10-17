module Translator where

import qualified Euterpea as Eu
import qualified Evaluator as E
import qualified Calc.Parser as P
import Data.Ratio



translate :: E.Exp -> Either String (Eu.Music Eu.Pitch)
translate (E.Play (E.Num n) e2) = case translateCompositions (fromIntegral n) e2 of
                                    Right (m,_) -> Right m
                                    Left f -> Left f
translate e = Left ("argument to translate is not music, but: "++(show e))


translateCompositions :: Int -> E.Exp -> Either String (Eu.Music Eu.Pitch,Int)
translateCompositions o (E.Tuple (E.Letter s) (E.Tuple (E.Num n1) (E.Num n2)))
 = do fun <- stringToLetterFun s
      return $ (fun o (n1%n2),o)

translateCompositions o (E.Cons _ n E.Dot e2) = translateCompositions (o+n) e2

translateCompositions o (E.Cons dim n e1 e2) = do (e1_music,o2) <- translateCompositions o e1
                                                  (e2_music,o3) <- translateCompositions (o2+n) e2
                                                  return $ (combinePitches dim e1_music e2_music,o3)

translateCompositions _ e = Left ("translateComps arg not E.Cons, but: "++(show e))


combinePitches :: P.Dim -> Eu.Music Eu.Pitch -> Eu.Music Eu.Pitch -> Eu.Music Eu.Pitch
combinePitches P.Serial m1 m2 = (Eu.:+:) m1 m2
combinePitches P.Parallel m1 m2 = (Eu.:=:) m1 m2


stringToLetterFun :: String -> Either String (Eu.Octave -> Eu.Dur -> Eu.Music Eu.Pitch)
stringToLetterFun "a" = Right Eu.a
--indexToLetterFun 1 = Eu.as
stringToLetterFun "b" = Right Eu.b
stringToLetterFun "c" = Right Eu.c
--indexToLetterFun 4 = Eu.cs
stringToLetterFun "d" = Right Eu.d
--indexToLetterFun 6 = Eu.ds
stringToLetterFun "e" = Right Eu.e
stringToLetterFun "f" = Right Eu.f
--indexToLetterFun 9 = Eu.fs
stringToLetterFun "g" = Right Eu.g
stringToLetterFun _ = Left "not a letter"

