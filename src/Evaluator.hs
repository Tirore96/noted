{-# LANGUAGE GADTs #-}
module Evaluator where

import qualified Euterpea as Eu
import qualified Data.Map as Map
import Data.Ratio
import qualified Calc.Parser as P
import Control.Monad.State.Lazy
import qualified Data.List as List
import qualified TypeChecker as T

type StateData = Map.Map String EvalTerm

type SEME a = StateT StateData (Either String) a

updateState :: String -> EvalTerm ->  SEME ()
updateState str t = StateT (\s -> Right ((),Map.insert str t s))

lookupVar :: String -> SEME EvalTerm
lookupVar str = StateT (\s -> case Map.lookup str s of 
                            Just t -> Right (t,s)
                            Nothing -> Left "bad variable lookup")

data Position = NumPos Integer | LetterPos Letter
type Letter = (Char,Signature) 
data Signature = Sharp | Flat | Natural
data NoteStruct = NoteStruct { 
                   duration :: Maybe Integer,
                   position :: Maybe Position,
                   octave   :: Maybe Int
                 }
data Notes = Note NoteStruct 
              | Notes P.CompType Notes Notes 

data EvalTerm = EConstant Integer
                | ENotes Notes
                | EMusic (Eu.Music Eu.Pitch)
                | EContext [(P.Label,EvalTerm)]
                | EPosition Position

evaluate :: P.Program -> Either String (Eu.Music Eu.Pitch)
evaluate program = let m = do mapM updateStore program
                              lookupVar "main"
                   in case runStateT m Map.empty of
                        Right (EMusic m,_) -> Right m
                        Left err -> Left err

updateStore :: P.Assignment -> SEME ()
updateStore (P.Assignment (var,_) term) = do
  t <- evaluateTerm term
  updateState var t
  
evaluateTerm :: P.Term -> SEME EvalTerm
evaluateTerm (P.Num (n,_)) =return (EConstant n)  -- might become ENote when sorroundings are known
evaluateTerm (P.Dur (n,_)) = return $ ENotes $ Note $ 
                               NoteStruct (Just n) Nothing Nothing
--evaluateTerm (P.Letter (s,_)) = undefined
evaluateTerm (P.Composition compType t1 t2) = do
  t1_temp_eval <- evaluateTerm t1 
  t2_temp_eval <- evaluateTerm t2
  let t1_eval = constToEnote t1_temp_eval
      t2_eval = constToEnote t2_temp_eval
  case (t1_eval,t2_eval) of
    (ENotes en1, ENotes en2) -> return (ENotes (Notes compType en1 en2 ))
    (EMusic m1,EMusic m2) -> case compType of
                              P.Serial -> return $ EMusic  ((Eu.:+:) m1 m2)
                              P.Parallel-> return $ EMusic ((Eu.:=:) m1 m2)


evaluateTerm (P.Variable (var,_))= lookupVar var
evaluateTerm (P.Context ctx) = let (labels,values) = unzip ctx
                               in let newValues = map evaluateCtxValue values
                                  in let newCtx = zip labels newValues
                                     in do return $ EContext newCtx
evaluateTerm (P.Application (P.ToNotes,_) terms) = do
  durations_eval <- evaluateTerm (terms!!0)
  positions_temp <- evaluateTerm (terms!!1)
  let positions_eval = constToEnote (positions_temp)
  case (durations_eval,positions_eval) of
    (ENotes durations,ENotes positions) -> return $ ENotes $ joinDurPos durations positions
    _ -> failM "failed"

evaluateTerm (P.Application (P.ToMusic,_) terms) = do
  notes_eval <- evaluateTerm (terms!!0)  --Not safe
  ctx_eval <- evaluateTerm (terms!!1)   --Not safe
  case (notes_eval,ctx_eval) of
    (ENotes notes,EContext ctx) -> 
       let ctx_map = Map.fromList ctx
       in let Just (EConstant octave) = Map.lookup P.Octave ctx_map    --Not safe
          in let notes_w_octave = addOctave notes (fromIntegral octave) --TODO
             in do case typeOfNotes notes of
                     T.Lettered-> return $ EMusic (buildMusic notes )
                     T.Numbered->let Just (EPosition (LetterPos l)) = Map.lookup P.Key ctx_map
                                 in let notes_w_o_l = numberedToLettered notes_w_octave l ---cut corner, not --handled sflats
                                    in return $ EMusic (buildMusic notes_w_o_l) 
    _ -> failM "failed"
evaluateCtxValue :: P.Term -> EvalTerm
--evaluateCtxValue (P.Variable (v,_)) = do evalTerm <- lookupVar v     --- variables not allowed in struct
--                                         return $ evaluateCtxValue evalTerm
evaluateCtxValue (P.Num (n,_)) = EConstant n
evaluateCtxValue (P.Letter (s,_)) = if length s == 1
                                      then EPosition (LetterPos (head s,Natural))
                                      else if s!!1 == '#'
                                             then EPosition (LetterPos (head s,Sharp))
                                             else EPosition (LetterPos (head s,Flat))

addOctave :: Notes -> Int -> Notes
addOctave (Notes compType n1 n2) octave = 
  Notes compType (addOctave n1 octave) (addOctave n2 octave)
addOctave (Note struct) octave = 
  let dur = duration struct
      pos = position struct
      oc = Just octave
  in Note (NoteStruct dur pos oc)

buildMusic :: Notes -> Eu.Music Eu.Pitch
buildMusic (Note struct) =
  let Just dur = duration struct
      Just oc = octave struct
      Just (LetterPos pos) = position struct --Not safe
  in (letterToNoteFun pos) oc (1%dur)    --For now ignore signature

buildMusic (Notes P.Serial en1 en2) = 
  let m1 = (buildMusic en1) 
      m2 = (buildMusic en2)
  in (Eu.:+:) m1 m2

buildMusic (Notes P.Parallel en1 en2) = 
  let m1 = (buildMusic en1) 
      m2 = (buildMusic en2)
  in (Eu.:=:) m1 m2


typeOfNotes :: Notes -> T.PosType
typeOfNotes (Note struct ) = case position struct of
                               Just (LetterPos _) ->  T.Lettered
                               _ ->  T.Numbered

typeOfNotes (Notes _ en _) = typeOfNotes en -- Recurse left


joinDurPos :: Notes -> Notes -> Notes 
joinDurPos (Notes compType dur_en1 dur_en2) (Notes _ pos_en1 pos_en2) = 
  Notes compType (joinDurPos dur_en1 pos_en1) (joinDurPos dur_en2 pos_en2)

joinDurPos (Note struct1) (Note struct2) =   --Assumes octave not used yet
  let dur = duration struct1
      pos = position struct2
  in Note $ NoteStruct dur pos Nothing 

joinDurPos (Note struct) (Notes P.Parallel n1 n2) =
  let n1_new = joinDurPos (Note struct) n1
      n2_new = joinDurPos (Note struct) n2
  in Notes P.Parallel n1_new n2_new


constToEnote :: EvalTerm -> EvalTerm
constToEnote (EConstant n) = ENotes (Note $  NoteStruct Nothing (Just (NumPos n)) Nothing)
constToEnote evalTerm = evalTerm

scale = ['c','d','e','f','g','a','h']
numToNote :: Integer -> Letter -> (Int,Letter)
numToNote n (key,sig) = let Just offset = List.elemIndex key scale   --possibly return tuple in the future
                        in let (octave,index)= quotRem (offset+(fromIntegral n))  7
                           in (octave,(scale!!index,sig))

numberedToLettered :: Notes -> Letter -> Notes
numberedToLettered (Note struct) letter = 
  let dur = duration struct
      Just (NumPos pos_num) = position struct  --not safe
      (oc_offset,c) = numToNote pos_num letter
  in let Just oc = octave struct
         pos = Just $ LetterPos c
     in Note $ NoteStruct dur pos (Just (oc + oc_offset))

numberedToLettered (Notes compType en1 en2) key = 
  Notes compType (numberedToLettered en1 key)  (numberedToLettered en2 key)

letterToNoteFun :: Letter -> Eu.Octave -> Eu.Dur -> Eu.Music Eu.Pitch
letterToNoteFun ('a',_) = Eu.a
letterToNoteFun ('c',_) = Eu.c
letterToNoteFun ('d',_) = Eu.d
letterToNoteFun ('e',_) = Eu.e
letterToNoteFun ('f',_) = Eu.f
letterToNoteFun ('g',_) = Eu.g
letterToNoteFun ('h',_) = Eu.b  

failM str = StateT (\state -> Left str)
