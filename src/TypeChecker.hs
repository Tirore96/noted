module TypeChecker where

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
            | TNotes 
            | TMusic
            | TContext [P.Label]
  deriving(Eq,Show)

data PosType = Numbered | Lettered

type StateData = Map String Type

type SEMT a = StateT StateData (Either String) a

updateState :: String -> Type ->  SEMT ()
updateState str t = StateT (\s -> Right ((),Map.insert str t s))

lookupVar :: String -> SEMT Type 
lookupVar str = StateT (\s -> case Map.lookup str s of 
                            Just t -> Right (t,s)
                            Nothing -> Left "bad variable lookup")


assertSameType :: Type -> Type -> SEMT ()
assertSameType t1 t2 = if t1 == t2
                         then return ()
                         else failM $ "failed asserting "++(show t1)++"=="++(show t2)


typeCheck :: P.Program -> Either String String--Nothing for valid program
typeCheck program = let m = do mapM updateStore program
                               main_type <- lookupVar "main"
                               assertSameType TMusic main_type 
                    in case runStateT m Map.empty of
                         Right _ -> Right "accepted"
                         Left err -> Left err

updateStore :: P.Assignment -> SEMT ()
updateStore (P.Assignment (var,_) term) = do term_type <- typeCheckTerm term
                                             updateState var term_type


typeCheckTerm :: P.Term -> SEMT Type
typeCheckTerm (P.Num _) = return TNum
typeCheckTerm (P.Dur _) = return TDur
typeCheckTerm (P.Composition P.Serial t1 t2) = do 
  t1_type <- typeCheckTerm t1
  t2_type <- typeCheckTerm t2
  case (t1_type,t2_type) of
   (TComp TNum n,TComp TNum n2) -> return (TComp TNum (n+n2))
   (TComp TNum n,TNum) -> return (TComp TNum (n+1))
   (TComp TDur n,TComp TDur n2) -> return (TComp TDur (n+n2))
   (TComp TDur n,TDur) -> return (TComp TDur (n+1))
   (TNum,TNum) -> return (TComp TNum 2)
   (TDur,TDur) -> return (TComp TDur 2)
   (TMusic,TMusic) -> return TMusic
   l -> failM ("not same typein composition, got: "++(show l))

typeCheckTerm (P.Composition P.Parallel t1 t2) = do 
  t1_type <- typeCheckTerm t1
  t2_type <- typeCheckTerm t2
  case (t1_type,t2_type) of
   (TComp TNum n,TComp TNum n2) -> return (TComp TNum n)
   (TComp TNum n,TNum) -> return (TComp TNum n)
   (TComp TDur n,TComp TDur n2) -> return (TComp TDur n)
   (TComp TDur n,TDur) -> return (TComp TDur n)
   (TNum,TNum) -> return (TComp TNum 1)
   (TDur,TDur) -> return (TComp TDur 1)
   (TMusic,TMusic) -> return TMusic
   l -> failM ("not same typein composition, got: "++(show l))


typeCheckTerm (P.Variable (var,_)) = lookupVar var    
typeCheckTerm (P.Context labelPairs) = do labels <- mapM typeCheckLabelPair labelPairs 
                                          return (TContext labels)

typeCheckTerm (P.Application (P.ToNotes,_) arguments) = do
  types <- mapM typeCheckTerm arguments
  case types of
    [TDur,TNum] -> return TNotes
    [TComp TDur n1,TComp TNum n2] -> if n1== n2
                                       then return TNotes
                                       else failM "size mismatch"
    _ -> failM ("expected something, got"++(show types ))

typeCheckTerm (P.Application (P.ToMusic,_) arguments) = do
  types <- mapM typeCheckTerm arguments
  case types of
    [TNotes,TContext [P.Key,P.Octave]] ->  return TMusic
    _ -> failM ("expected something, got"++(show types))


typeCheckTerm _ = undefined 


typeCheckApplication arguments expected_types return_type = do
  actual_types <- mapM typeCheckTerm arguments
  assertArgumentSize actual_types (length actual_types)
  mapM (\(expected,actual) -> assertSameType expected actual)  
       (zip expected_types actual_types)
  return return_type


typeCheckLabelPair :: (P.Label,P.Term) -> SEMT P.Label
typeCheckLabelPair (P.Key,P.Letter _) = return P.Key 
typeCheckLabelPair (P.Key,val) = failM "Key label is associated with non-Note"

typeCheckLabelPair (P.Octave,P.Num _) = return P.Octave
typeCheckLabelPair (P.Octave,val) = failM "Octave label is associated with non-Num"

assertArgumentSize :: [Type] -> Int -> SEMT ()
assertArgumentSize args n = if length args == n
                              then return ()
                              else failM ("expected "++(show n)++" arguments, receieved "++(show $ length args))

failM str = StateT (\state -> Left str)
