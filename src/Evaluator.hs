{-# LANGUAGE GADTs #-}
module Evaluator where

import qualified Euterpea as Eu
import qualified Data.Map.Strict as Map
import Data.Ratio
import qualified Calc.Parser as P
import Control.Monad.State.Lazy
import qualified Data.List as List
import qualified TypeChecker as T
import Debug.Trace

type StateData = Map.Map String EvalTerm

type SEME a = StateT StateData (Either String) a

updateState :: String -> EvalTerm ->  SEME ()
updateState str t = StateT (\s -> Right ((),Map.insert str t s))

lookupVar :: String -> SEME EvalTerm
lookupVar str = StateT (\s -> case Map.lookup str s of 
                            Just t -> Right (t,s)
                            Nothing -> Left "bad variable lookup")

data Bindings = PosBind Position | DurBind Integer 
  deriving(Show,Eq)

data Position = NumPos Integer | LetterPos Letter
  deriving(Show,Eq)
type Letter = (Char,Signature) 
data Signature = Sharp | Flat | Natural
  deriving(Show,Eq)
data NoteStruct = NoteStruct { 
                   duration :: Maybe Integer,
                   position :: Maybe Position,
                   octave   :: Maybe Int
                 }
  deriving(Show,Eq)

data SymbolicNoteStruct = SymbolicNoteStruct { 
                   sDuration :: Maybe (Either Integer Symbolic),
                   sPosition :: Maybe (Either Position Symbolic),
                   sOctave   :: Maybe Int
                 }
  deriving(Show,Eq)

data Symbolic = SymVar String   -- Later add more symbolic expressions
  deriving(Show,Eq)

data TreeComp a = Leaf a
              | TreeComp P.CompType (TreeComp a) (TreeComp a)
  deriving(Show,Eq)

data FlatComp a = FlatList P.CompType [FlatComp a] | FlatBase a
  deriving(Show)

--data FlatPattern = FlatPattern (FlatComp SymbolicNoteStruct) (FlatComp SymbolicNoteStruct)
--  deriving(Show)

data EvalTerm = EConstant Integer
                | ETreeComp (TreeComp NoteStruct)
                | EMusic (Eu.Music Eu.Pitch)
                | EContext [(P.Label,EvalTerm)]
                | EPosition Position
                | EPattern (FlatComp SymbolicNoteStruct) (FlatComp SymbolicNoteStruct)



evaluate :: P.Program -> Either String (Eu.Music Eu.Pitch)
evaluate program = let m = do mapM updateStore program
                              lookupVar "main"
                   in case runStateT m Map.empty of
                        Right (EMusic m,_) -> Right m
                        Left err -> Left err

--Only for testing
evaluateTest program = let m = do mapM updateStore program
                                  lookupVar "main"
                       in case runStateT m Map.empty of
                            Right (val,_) -> Right val 
                            Left err -> Left err


updateStore :: P.Assignment -> SEME ()
updateStore (P.Assignment (var,_) term) = do
  t <- evaluateTerm term
  updateState var t
  
evaluateTerm :: P.Term -> SEME EvalTerm
evaluateTerm (P.Num (n,_)) =return (EConstant n)  -- might become ENote when sorroundings are known
evaluateTerm (P.Dur (n,_)) = return $ ETreeComp $ Leaf $ 
                               NoteStruct (Just n) Nothing Nothing
--evaluateTerm (P.Letter (s,_)) = undefined
evaluateTerm (P.Composition compType t1 t2) = do
  t1_temp_eval <- evaluateTerm t1 
  t2_temp_eval <- evaluateTerm t2
  let t1_eval = constToEnote t1_temp_eval
      t2_eval = constToEnote t2_temp_eval
  case (t1_eval,t2_eval) of
    (ETreeComp en1, ETreeComp en2) -> let comp = TreeComp compType en1 en2
                                in return $ ETreeComp ((rebuildComposition . flattenTreeComp) comp)
    (EMusic m1,EMusic m2) -> case compType of
                              P.Serial -> return $ EMusic  ((Eu.:+:) m1 m2)
                              P.Parallel-> return $ EMusic ((Eu.:=:) m1 m2)   
   --cases for every type that can be composed (arrows)

evaluateTerm (P.Variable (var,_))= lookupVar var
evaluateTerm (P.Context ctx) = let (labels,values) = unzip ctx
                               in let newValues = map evaluateCtxValue values
                                  in let newCtx = zip labels newValues
                                     in do return $ EContext newCtx

evaluateTerm (P.Pattern pIn pOut) = do
  treeSymPIn <- termToSymTreeComp pIn
  treeSymPOut <- termToSymTreeComp pOut
  return $ EPattern (flattenTreeComp treeSymPIn) (flattenTreeComp treeSymPOut)

evaluateTerm (P.Application (P.ToNotes,_) terms) = do
  durations_eval <- evaluateTerm (terms!!0)
  positions_temp <- evaluateTerm (terms!!1)
  let positions_eval = constToEnote (positions_temp)
  case (durations_eval,positions_eval) of
    (ETreeComp durations,ETreeComp positions) -> return $ ETreeComp $ joinDurPos durations positions
    _ -> failM "failed"

evaluateTerm (P.Application (P.ToMusic,_) terms) = do
  notes_eval <- evaluateTerm (terms!!0)  --Not safe
  ctx_eval <- evaluateTerm (terms!!1)   --Not safe
  case (notes_eval,ctx_eval) of
    (ETreeComp notes,EContext ctx) -> 
       let ctx_map = Map.fromList ctx
       in let Just (EConstant octave) = Map.lookup P.Octave ctx_map    --Not safe
          in let notes_w_octave = addOctave notes (fromIntegral octave) --TODO
             in do case typeOfNotes notes of
                     T.Lettered-> return $ EMusic (buildMusic notes )
                     T.Numbered->let Just (EPosition (LetterPos l)) = Map.lookup P.Key ctx_map
                                 in let notes_w_o_l = numberedToLettered notes_w_octave l ---cut corner, not --handled sflats
                                    in return $ EMusic (buildMusic notes_w_o_l) 
    _ -> failM "failed"

evaluateTerm (P.Application (P.Transform,_) terms) = do
  input <- evaluateTerm (terms!!0)
  pattern <- evaluateTerm (terms!!1)
  case (input,pattern) of
    (ETreeComp comp,(EPattern pIn pOut)) -> 
         let flattened = flattenTreeComp comp
         in let (_,_,transformed) = patternMatchSub flattened pIn pOut
            in return $ ETreeComp $ rebuildComposition transformed
    _ -> failM "got something unexpected"
 
--    (EPattern P.Serial p1 p2) -> let input2 = transform input ((makePatternFun id) p1) --Monad?
--                                      in let input3 = transform input2 ((makePatternFun id) p2)
--                                         in evaluateTerm input3
--    (EPattern P.Parallel p1 p2) -> let input2 = transform input ((combinePatterns id ) p1 p2)
--                                        in evaluateTerm input2

termToSymTreeComp :: P.Term -> SEME (TreeComp SymbolicNoteStruct)
termToSymTreeComp (P.Dur (n,_)) =  return $ Leaf $ SymbolicNoteStruct (Just (Left n)) Nothing Nothing
termToSymTreeComp (P.Num (n,_)) =  return $ Leaf $ SymbolicNoteStruct Nothing (Just (Left (NumPos n))) Nothing
termToSymTreeComp (P.Variable (s,_)) = case head (tail s) of
                                      'd' -> return $ Leaf $ SymbolicNoteStruct (Just (Right (SymVar s))) Nothing Nothing
                                      'p' -> return $ Leaf $ SymbolicNoteStruct Nothing (Just (Right (SymVar s))) Nothing
                                      _ -> failM "unexpected" --Later add "C" for chord
termToSymTreeComp (P.Composition compType t1 t2) = do symT1 <- termToSymTreeComp t1
                                                      symT2 <- termToSymTreeComp t2
                                                      return $ TreeComp compType symT1 symT2
                                                     
termToSymTreeComp var = failM ("did not expect"++(show var))




flattenTreeComp :: TreeComp a -> FlatComp a 
flattenTreeComp (TreeComp compType t1 t2) = 
  let flattened_t1 = flattenTreeComp t1
      flattened_t2 = flattenTreeComp t2
      makeFlatListMember flat_notes = 
          case flat_notes of
            FlatList compType1 l -> 
                 if compType == compType1 
                   then l
                   else [flat_notes]
            base -> [base]
  in let t1_member = makeFlatListMember flattened_t1
         t2_member = makeFlatListMember flattened_t2
     in FlatList compType (t1_member++t2_member)

flattenTreeComp (Leaf struct) = FlatBase struct

rebuildComposition :: FlatComp a -> TreeComp a
rebuildComposition (FlatList compType l) = 
  let left = rebuildComposition $ head l
      rest = tail l
  in foldl (\comp flatComp -> let right = rebuildComposition flatComp
                              in TreeComp compType comp right ) (left)  rest

rebuildComposition (FlatBase struct) = Leaf struct

--transformFlatComp :: FlatComp -> FlatPattern -> FlatComp
--transformFlatComp (FlatBase struct) (FlatPattern pIn pOut) =
--  patternMatchSub struct pIn pOut

--transformFlatComp (FlatList compType l) (FlatPattern pIn pOut) =
--  let l_transformed = map transformFlatComp l (FlatPattern pIn pOut)
--  in case pIn of
--       FlatBase _ -> FlatList compType l_transformed  -- only matches base types which have been handled above
--       FlatList compType1 l1 -> if compType == compType1
--                                  then pOutList = case pOut of
--                                                    Flatbase b -> [b]
--                                                    FlatList compType2 l2 -> if compType == compType2 
--                                                                               then l2
--                                                                               else [pOut]
--                                substituteList l_transformed l1 pOut
--                                else FlatList compType l_transformed -- if compType doesn't match, pattern can't match


patternMatchHelper key symKey const struct symStruct = 
  case key struct of
       Nothing -> case symKey symStruct of
                    Nothing -> (True,False,Map.empty)
                    _ -> (False,True,Map.empty)

       Just n -> case symKey symStruct of
                   Just (Left symN) -> (n==symN,True,Map.empty)
                   Just (Right (SymVar s)) -> (True,True,Map.insert s (const n) Map.empty)
                   Nothing -> (False,False,Map.empty)

patternMatchSubStruct :: NoteStruct -> SymbolicNoteStruct -> FlatComp SymbolicNoteStruct -> (Bool,Map.Map String Bindings,FlatComp NoteStruct)
patternMatchSubStruct struct symStructIn pOut = 
  let (matched1,hasDur,myMap1) = patternMatchHelper duration sDuration DurBind struct symStructIn
      (matched2,hasPos,myMap2) = patternMatchHelper position sPosition PosBind struct symStructIn

  in let (matched,myMap) =  case (hasDur,hasPos) of
                              (True,True) -> (matched1 && matched2,Map.union  myMap1 myMap2)
                              (True,False) -> (matched1,myMap1)
                              (False,True)-> (matched2,myMap) --Dobbelt check
     in if matched 
          then (True,myMap,substitute pOut myMap)
          else (False,Map.empty,FlatBase struct)

substituteStruct :: SymbolicNoteStruct -> (Map.Map String Bindings )  -> NoteStruct
substituteStruct symStruct myMap =
  let dur = case sDuration symStruct of
              Just (Right (SymVar s)) -> case Map.lookup s myMap of 
                                           Just (DurBind d) -> Just d
                                           Nothing -> Nothing
              Just (Left n) -> Just n
              Nothing -> Nothing

      pos = case sPosition symStruct of
              Just (Right (SymVar s)) -> case Map.lookup s myMap of 
                                           Just (PosBind d) -> Just d
                                           Nothing -> Nothing
              Just (Left n) -> Just n
              Nothing -> Nothing

     in NoteStruct dur pos Nothing

substitute :: FlatComp SymbolicNoteStruct -> Map.Map String Bindings -> FlatComp NoteStruct
substitute (FlatBase symStruct) myMap = FlatBase (substituteStruct symStruct myMap)
substitute (FlatList compType l) myMap = FlatList compType $ map (\elem -> substitute elem myMap) l 


patternMatchSub :: (FlatComp NoteStruct) ->  (FlatComp SymbolicNoteStruct) -> (FlatComp SymbolicNoteStruct) -> (Bool,Map.Map String Bindings ,FlatComp NoteStruct)
patternMatchSub (FlatBase struct) (FlatBase symStructIn) pOut = let (bool,myMap,newStruct) = patternMatchSubStruct struct symStructIn pOut
                                                                in (bool,myMap,newStruct)
patternMatchSub (FlatBase struct) (FlatList _ _) _ = (False,Map.empty,FlatBase struct) -- Can't match big pattern to small instance
patternMatchSub (FlatList compType1 l1) pIn pOut = 
  let (bool,myMap,subbed_l1) = foldl (\(bool1,myMap1,l) elem -> 
                             let (bool2,myMap2,flatComp) = patternMatchSub elem pIn pOut
                             in (bool1&&bool2,Map.union myMap1 myMap2,l++[flatComp])) (True,Map.empty,[]) l1-- Can't match big pattern to small instance
  in case pIn of 
       FlatBase _ -> (bool,myMap,FlatList compType1 subbed_l1)
       FlatList compType2 l2 -> if compType1 /= compType2
                                 then (bool,myMap,FlatList compType1 subbed_l1) --can't abstract over members with wrong compType
                                 else let subbed_l2 = slidePatternOverList compType1 l1 l2 pOut
                                      in (True,Map.empty,FlatList compType1 subbed_l2) ---Maybe problem, should myMap and myMap2 be joined?
                                                                                       ---Maybe another program, how far up should Maps be shared? Only same level right?


neutralFlatComp = FlatBase $ SymbolicNoteStruct Nothing Nothing Nothing

patternMatchSubList :: P.CompType -> [FlatComp NoteStruct] ->  [FlatComp SymbolicNoteStruct] -> FlatComp SymbolicNoteStruct -> (Bool,[FlatComp NoteStruct])
patternMatchSubList compType flatCompL flatCompLIn pOut = 
  let list_n_match = zip flatCompL flatCompLIn
  in let (matched,myMap) = foldl (\(bool1,myMap1)(elem,match) -> 
                                let (bool2,myMap2,_) = patternMatchSub elem match neutralFlatComp --Only interested in the the boolean
                                in (bool1&&bool2,Map.union myMap1 myMap2)) (True,Map.empty) list_n_match
     in if matched 
          then let substitution = substitute pOut myMap  --what to do with the store for pOut? ----->
               in case substitution of 
                 FlatBase b -> (True,[substitution])
                 FlatList compType2 l2 -> if compType /= compType2
                                           then (True,[substitution])
                                           else (True,l2)

          else (False,flatCompL)
   
                      
slidePatternOverList :: P.CompType -> [FlatComp NoteStruct] -> [FlatComp SymbolicNoteStruct] -> FlatComp SymbolicNoteStruct -> [FlatComp NoteStruct]
slidePatternOverList compType flatCompL flatCompLIn pOut = slidePatternOverListHelper compType flatCompL flatCompLIn pOut (length flatCompL) 

slidePatternOverListHelper compType flatCompL flatCompLIn pOut n =
  let (bool,flatCompL2) = patternMatchSubList compType (take n flatCompL) flatCompLIn pOut
  in if bool
      then let n2 = length flatCompL2
           in flatCompL2++(slidePatternOverListHelper compType (drop n2 flatCompL) flatCompLIn pOut (n-n2))
      else (head flatCompL):(slidePatternOverListHelper compType (tail flatCompL) flatCompLIn pOut (n-1))


--substituteList :: [FlatComp NoteStruct] -> [FlatComp SymbolicNoteStruct] [(Integer,Map.String NoteStruct)]-> [FlatComp NoteStruct]
--substituteList l replace_list index_maps = substituteList l replace_list index_maps (length l)
--  
--substituteListHelper l replace_list ((index,myMap):rest) l_length =
--
--patternMatch (FlatBase struct) (FlatBase struct) = 
 

--transform :: TreeComp -> (Notes -> Notes) -> Notes
--transform (P.Composition compType comp1 comp2) patternFun = 
--  let transformed_comp1 = patternFun comp1
--      transformed_comp2 = patternFun comp2
--  in P.Composition compType transformed_comp1 transformed_comp2
  
--leafFun :: TreeComp -> (Notes -> Notes)
--leafFun pattern = makePatternFun pattern id
--
--makePatternFun :: TreeComp -> ((Notes -> Notes) -> TreeComp -> Notes)
--makePatternFun (P.Pattern match ret)= \fun term -> if term == match
--                                                   then ret
--                                                   else fun term
--
--combinePatterns :: TreeComp -> TreeComp -> ((Notes -> Notes) -> TreeComp -> Notes)
--combinePatterns pattern1 pattern2 = let fun1 = makePatternFun pattern1
--                                        fun2 = makePatternFun pattern2
--                                    in fun1 fun2

evaluateCtxValue :: P.Term -> EvalTerm
evaluateCtxValue (P.Num (n,_)) = EConstant n
evaluateCtxValue (P.Letter (s,_)) = if length s == 1
                                      then EPosition (LetterPos (head s,Natural))
                                      else if s!!1 == '#'
                                             then EPosition (LetterPos (head s,Sharp))
                                             else EPosition (LetterPos (head s,Flat))

addOctave :: TreeComp NoteStruct-> Int -> TreeComp NoteStruct
addOctave (TreeComp compType n1 n2) octave = 
  TreeComp compType (addOctave n1 octave) (addOctave n2 octave)
addOctave (Leaf struct) octave = 
  let dur = duration struct
      pos = position struct
      oc = Just octave
  in Leaf (NoteStruct dur pos oc)

buildMusic :: TreeComp NoteStruct-> Eu.Music Eu.Pitch
buildMusic (Leaf struct) =
  let Just dur = duration struct
      Just oc = octave struct
      Just (LetterPos pos) = position struct --Not safe
  in (letterToNoteFun pos) oc (1%dur)    --For now ignore signature

buildMusic (TreeComp P.Serial en1 en2) = 
  let m1 = (buildMusic en1) 
      m2 = (buildMusic en2)
  in (Eu.:+:) m1 m2

buildMusic (TreeComp P.Parallel en1 en2) = 
  let m1 = (buildMusic en1) 
      m2 = (buildMusic en2)
  in (Eu.:=:) m1 m2


typeOfNotes :: TreeComp NoteStruct-> T.PosType
typeOfNotes (Leaf struct ) = case position struct of
                               Just (LetterPos _) ->  T.Lettered
                               _ ->  T.Numbered

typeOfNotes (TreeComp _ en _) = typeOfNotes en -- Recurse left


joinDurPos :: TreeComp NoteStruct-> TreeComp NoteStruct-> TreeComp NoteStruct
joinDurPos (TreeComp compType dur_en1 dur_en2) (TreeComp _ pos_en1 pos_en2) = 
  TreeComp compType (joinDurPos dur_en1 pos_en1) (joinDurPos dur_en2 pos_en2)

joinDurPos (Leaf struct1) (Leaf struct2) =   --Assumes octave not used yet
  let dur = duration struct1
      pos = position struct2
  in Leaf $ NoteStruct dur pos Nothing 

joinDurPos (Leaf struct) (TreeComp P.Parallel n1 n2) =
  let n1_new = joinDurPos (Leaf struct) n1
      n2_new = joinDurPos (Leaf struct) n2
  in TreeComp P.Parallel n1_new n2_new


constToEnote :: EvalTerm -> EvalTerm
constToEnote (EConstant n) = ETreeComp (Leaf $  NoteStruct Nothing (Just (NumPos n)) Nothing)
constToEnote evalTerm = evalTerm

scale = ['c','d','e','f','g','a','h']
numToNote :: Integer -> Letter -> (Int,Letter)
numToNote n (key,sig) = let Just offset = List.elemIndex key scale   --possibly return tuple in the future
                        in let (octave,index)= quotRem (offset+(fromIntegral n))  7
                           in (octave,(scale!!index,sig))

numberedToLettered :: TreeComp NoteStruct-> Letter -> TreeComp NoteStruct
numberedToLettered (Leaf struct) letter = 
  let dur = duration struct
      Just (NumPos pos_num) = position struct  --not safe
      (oc_offset,c) = numToNote pos_num letter
  in let Just oc = octave struct
         pos = Just $ LetterPos c
     in Leaf $ NoteStruct dur pos (Just (oc + oc_offset))

numberedToLettered (TreeComp compType en1 en2) key = 
  TreeComp compType (numberedToLettered en1 key)  (numberedToLettered en2 key)

letterToNoteFun :: Letter -> Eu.Octave -> Eu.Dur -> Eu.Music Eu.Pitch
letterToNoteFun ('a',_) = Eu.a
letterToNoteFun ('c',_) = Eu.c
letterToNoteFun ('d',_) = Eu.d
letterToNoteFun ('e',_) = Eu.e
letterToNoteFun ('f',_) = Eu.f
letterToNoteFun ('g',_) = Eu.g
letterToNoteFun ('h',_) = Eu.b  

failM str = StateT (\state -> Left str)
