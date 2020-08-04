module TypeChecker where
--Possibly type check for repeating labels. ie {quantization=10;quantization=20} shouldn't be allowed

import Data.Map as Map
import Control.Monad.State.Lazy
import Data.Set as Set
import Debug.Trace

import Calc.Lexer (AlexPosn)
import qualified Calc.Parser as P
import qualified Euterpea  as Eu (Dur)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Ratio

data Type = TDur
            | TNum
            | TComp Type Integer
            | TFun [Type] Type
            | TPattern Type Type
            | TNotes 
            | TMusic
            | TContext [P.Label]
  deriving(Eq,Show)

data PosType = Numbered | Lettered  -- Should never write octave position on note

type StateData = Map String Type

type SEMT a = StateT StateData (Either String) a

updateState :: String -> Type ->  SEMT ()
updateState str t = StateT (\s -> Right ((),Map.insert str t s))

lookupVar :: String -> SEMT Type 
lookupVar str = StateT (\s -> case Map.lookup str s of 
                            Just t -> Right (t,s)
                            Nothing -> Left ("The variable: "++str++", is unbound" ))


assertEquals :: (Eq a,Show a) => a -> a -> SEMT ()
assertEquals t1 t2 = if t1 == t2
                         then return ()
                         else failM $ "failed asserting "++(show t1)++"=="++(show t2)

typeCheck :: P.Program -> Either String String--Nothing for valid program
typeCheck program = let m = do mapM updateStore program
                               main_type <- lookupVar "main"
                               assertEquals TMusic main_type 
                    in case runStateT m Map.empty of
                         Right _ -> Right "accepted"
                         Left err -> Left err

updateStore :: P.Assignment -> SEMT ()
updateStore (P.Assignment (var,_) term) = do term_type <- typeCheckTerm term
                                             updateState var term_type


typeCheckTerm :: P.Term -> SEMT Type
typeCheckTerm (P.Num _) = return TNum
typeCheckTerm (P.Dur _) = return TDur
typeCheckTerm (P.ArrowVar _) = undefined

typeCheckTerm (P.Composition compType comp1 comp2) = do 
  t1_type <- typeCheckTerm comp1
  t2_type <- typeCheckTerm comp2
  typeCheckComp compType t1_type t2_type 

typeCheckTerm(P.Pattern t1 t2) = do t1_type <- typeCheckTerm t1
                                    t2_type <- typeCheckTerm t2
                                    return $ TPattern t1_type t2_type

typeCheckTerm (P.Variable (var,_)) = lookupVar var    
typeCheckTerm (P.Context labelPairs) = do labels <- mapM typeCheckLabelPair labelPairs 
                                          return (TContext labels)

typeCheckTerm (P.Application (P.ToNotes,_) arguments) = do
  types <- mapM typeCheckTerm arguments
  case types of
    [TDur,TNum] -> return TNotes
    [TComp TDur n1,TComp TNum n2] -> do assertEquals n1 n2
                                        return TNotes
    _ -> failM ("expected something, got"++(show types ))

typeCheckTerm (P.Application (P.ToMusic,_) arguments) = do
  types <- mapM typeCheckTerm arguments
  case types of
    [TNotes,TContext [P.Key,P.Octave]] ->  return TMusic
    _ -> failM ("expected something, got"++(show types))

typeCheckTerm (P.Application (P.Transform,_) arguments) = do
  types <- mapM typeCheckTerm arguments
  case types of
    [const1,TPattern t1 t2] -> do assertEquals const1 t1
                                  return t2 
    _ -> failM ("expected something, got"++(show types))

typeCheckTerm t = traceShow t (return TMusic)


isCompBaseType :: Type -> SEMT ()
isCompBaseType TDur = return ()
isCompBaseType TNum = return ()
isCompBaseType TMusic = return ()

isCompBaseType (TPattern _ _) = return ()
isCompBaseType l = failM $ "error 2"++(show l)

compCaseHandler compType (TComp const1 n1) (TComp const2 n2) helper =
  helper compType const1 const2 n1 n2

compCaseHandler compType (TComp const1 n1) const2 helper = do
  isCompBaseType const2
  helper compType const1 const2 n1 1

compCaseHandler compType const1 (TComp const2 n2) helper = do
  isCompBaseType const1
  helper compType const1 const2 1 n2

compCaseHandler compType const1 const2 helper = do
  isCompBaseType const1
  isCompBaseType const2
  helper compType const1 const2 1 1

typeCheckComp compType comp1 comp2  = compCaseHandler compType comp1 comp2 typeCheckCompHelper

--For patterns
typeCheckCompHelper compType (TPattern t1 t2) (TPattern t3 t4) n1 n2 =
  case compType of
    P.Serial -> do assertEquals t2 t3
                   return $ TComp (TPattern t1 t4) (n1+n2)
    P.Parallel -> do assertEquals (TPattern t1 t2) (TPattern t3 t4)
                     return $ TComp (TPattern t1 t2) n1

--For non-patterns
typeCheckCompHelper compType TMusic TMusic n1 n2 =
  return TMusic

typeCheckCompHelper compType const1 const2 n1 n2 =
  case compType of
    P.Serial -> do assertEquals const1 const2
                   return $ TComp const1 (n1+n2)
    P.Parallel -> do assertEquals const1 const2
                     return $ TComp const1 (n1)

typeCheckApplication arguments expected_types return_type = do
  actual_types <- mapM typeCheckTerm arguments
  assertEquals (length actual_types) (length actual_types)
  mapM (\(expected,actual) -> assertEquals expected actual)  
       (zip expected_types actual_types)
  return return_type


typeCheckLabelPair :: (P.Label,P.Term) -> SEMT P.Label
typeCheckLabelPair (P.Key,P.Letter _) = return P.Key 
typeCheckLabelPair (P.Key,val) = failM "Key label is associated with non-Note"

typeCheckLabelPair (P.Octave,P.Num _) = return P.Octave
typeCheckLabelPair (P.Octave,val) = failM "Octave label is associated with non-Num"


failM str = StateT (\state -> Left str)
