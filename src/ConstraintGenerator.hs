{-# LANGUAGE GADTs #-}
module ConstraintGenerator where

import qualified Calc.Parser as P
import qualified Data.Map as Map
import Control.Monad.State.Lazy

data StateData = StateData {constraints :: [Constraint],
                            currentId :: Integer,
                            gamma :: Map.Map String TypedTerm
                           }

type TypedTerm = (Type,P.Term)

structAddConstraints :: [Constraint] -> StateData -> StateData
structAddConstraints newConstraints stateData = 
  let oldConstraints = constraints stateData
      oldId = currentId stateData
      oldGamma = gamma stateData
  in StateData (oldConstraints++newConstraints) oldId oldGamma 

structGetId :: StateData -> (StateData,Integer)
structGetId stateData =
  let oldConstraints = constraints stateData
      oldId = currentId stateData
      oldGamma = gamma stateData
  in (StateData oldConstraints (oldId+1) oldGamma,oldId)

structUpdateGamma :: String -> TypedTerm -> StateData -> StateData
structUpdateGamma var t stateData = 
  let oldConstraints = constraints stateData
      oldId = currentId stateData
      oldGamma = gamma stateData
  in StateData oldConstraints oldId (Map.insert var t oldGamma)


type ConstraintM a = State StateData a

addConstraints :: [Constraint] -> ConstraintM () 
addConstraints cs = do myState <- get
                       put (structAddConstraints cs myState)
                       return ()

getId :: ConstraintM Integer
getId = do myState <- get 
           let (newState,n) = structGetId myState
           put newState
           return $ n

updateGamma :: String -> TypedTerm -> ConstraintM ()
updateGamma var t = do myState <- get
                       put (structUpdateGamma var t myState)
                       return ()

lookUpGamma :: String -> ConstraintM TypedTerm
lookUpGamma var = do myState <- get
                     case Map.lookup var (gamma myState) of
                      Just t -> return t
                      Nothing -> return ((BNone, Single),P.TVar (var,(0,0)))

type Type = (BaseType,CardinalType)

data BaseType = BFresh Integer
            | BIndex
            | BLetter            
            | BColor
            | BScale
            | BOctave
            | BPlacement
            | BDur
            | BMusic
            | BNone
            | BAny
  deriving(Eq,Show)

data CardinalType = Single
            | Multiple P.CompType [CardinalType]
            | CFresh Integer
            | Times CardinalType CardinalType
            | CNone
  deriving(Eq,Show)

sameType :: Type -> Type -> Bool
sameType (t1,t2) (t3,t4) = sameBaseType t1 t3 && sameCardinalType t2 t4

sameBaseType :: BaseType -> BaseType -> Bool
sameBaseType BAny _ = True
sameBaseType _ BAny = True
sameBaseType BNone _= False
sameBaseType _ BNone = False
sameBaseType t1 t2 = t1 == t2


sameCardinalType :: CardinalType -> CardinalType -> Bool
sameCardinalType t1 t2 = case evalTimes t1 t2 of
                           Just _ -> True
                           Nothing -> False

evalTimes :: CardinalType -> CardinalType -> Maybe CardinalType
evalTimes Single Single = Just Single
evalTimes mult@(Multiple _ _) Single = Just mult
evalTimes Single mult@(Multiple _ _) = Just mult
evalTimes (Times t1 t2 ) t3 = do evaled <-  evalTimes t1 t2
                                 evalTimes evaled t3

evalTimes t1 (Times t2 t3 ) = do evaled <-  evalTimes t2 t3
                                 evalTimes evaled t1

evalTimes t1@(Multiple ct1 cs1) t2@(Multiple ct2 cs2) =
  if ct1 /= ct2
    then case (ct1,ct2) of
           (P.Serial,P.Parallel) -> do new_cs <- mapM (\cs1_elem -> evalTimes cs1_elem t2) cs1
                                       return $ Multiple P.Serial new_cs
           (P.Parallel,P.Serial) -> do new_cs <- mapM (\cs2_elem -> evalTimes t1 cs2_elem) cs2
                                       return $ Multiple P.Serial new_cs
    else case (cs1,cs2) of
           ([],[]) -> Just Single  --just to give something
           (_:_,[]) -> Nothing
           ([],_:_) -> Nothing
           _ -> let zipped = zip cs1 cs2 
                in do new_cs <- mapM (\(cs1_elem,cs2_elem) -> evalTimes cs1_elem cs2_elem) zipped
                      return $ Multiple ct1 new_cs

evalTimes _ _ = Nothing



flatListShape :: P.Term -> CardinalType
flatListShape (P.TFlatList ((cs,terms),_)) = Multiple cs (map flatListShape terms)
flatListShape _ = Single

type Expected = Type 
type Actual = Type

data Constraint = Equals Expected Actual P.Term
  deriving(Eq)

instance Show Constraint where
  show (Equals t1 t2 term) = (show t1) ++ " = " ++ (show t2) ++ ",from " ++ (show term)

generateConstraints :: P.Program -> [Constraint]
generateConstraints program = 
  let myState = StateData [] 0 Map.empty
  in let (_,newState) = runState (genConstraints program) myState 
     in constraints newState

genConstraints :: P.Program -> ConstraintM ()
genConstraints program = do mapM_ genConstraint program
                            (main_type,term) <-  lookUpGamma "main"
                            addConstraints [Equals (BMusic, Single) main_type term]
                              

genConstraint :: P.Line -> ConstraintM ()
genConstraint (P.Assignment (var,_) term) = do
  term_t <- genConstraintT term
  updateGamma var term_t

genConstraint  _ = return () --Don't type check import statement


genConstraintT :: P.Term -> ConstraintM TypedTerm 
genConstraintT term@(P.TIndex _) = return ((BIndex, Single),term)
genConstraintT term@(P.TDur _) = return $ ((BDur, Single),term)
genConstraintT term@(P.TLetter _) = return $ ((BLetter, Single),term)
genConstraintT term@(P.TOctave _) = return $ ((BOctave, Single),term)
genConstraintT term@(P.TColor _) = return $ ((BColor, Single), term)
genConstraintT term@(P.TFlatList ((_,terms),_)) = do
  types <- mapM genConstraintT terms
  case types of
    [] -> return ((BNone, Single),term)  -- Shouldn't be possible so allow any type, and expect some other constraint will fail
    (_:_) -> do myId <- getId
                let expected_type = (BFresh myId, Single)
                    new_cons = map (\(t,p2) -> Equals expected_type t p2) types 
                addConstraints new_cons
                let compShape = flatListShape term
                return $ ((BFresh myId, compShape), term)

genConstraintT (P.TVar (str,_)) = lookUpGamma str

genConstraintT term@(P.TWith ((P.WithDur,term1,term2),_)) = do
  withXConstraints term term1 term2 BMusic BPlacement BDur

genConstraintT term@(P.TWith ((P.WithOctave,term1,term2),_)) =
  withXConstraints term term1 term2 BPlacement BLetter BOctave

genConstraintT term@(P.TWith ((P.WithScale ,term1,term2),_)) =
  withXConstraints term term1 term2 BLetter BIndex BScale

genConstraintT term@(P.TWith ((P.WithColor,term1,term2),_)) =
  withXConstraints term term1 term2 BScale BLetter BColor

withXConstraints :: P.Term -> P.Term -> P.Term -> BaseType -> BaseType -> BaseType -> ConstraintM TypedTerm 
withXConstraints term term1 term2 retType type1 type2 = do
  (t1,_) <- genConstraintT term1
  (t2,_) <- genConstraintT term2
  let c1 = Equals (type1,snd t2) t1 term
      c2 = Equals (type2,snd t1) t2 term
      ret = (retType,Times (snd t1) (snd t2))
  addConstraints [c1,c2]
  return (ret,term)

--type SolveResult = Either String [Constraint]
--
--solve :: [Constraint] -> Either String [Constraint]
--solve cons = do 
--  potential_solution <- solveConstraints cons 
--  if verifySolution potential_solution
--    then return $ potential_solution
--    else solve potential_solution
--
----solveDeferedTests 
--verifySolution :: [Constraint] -> Bool
--verifySolution [] = True
--verifySolution _ = False
--
--solveConstraints :: [Constraint] -> SolveResult
--solveConstraints [] = return []
--solveConstraints (first@(Equals expected actual _):rest) = --How to handle Equals (expected:fresh) (acftual:t)?
--  if sameType expected actual--Maybe subtype here?
--    then solveConstraints rest
--    else solveConstraintsHelper (first:rest)
--
----Substitute
--
--solveConstraintsHelper ::  [Constraint]  -> Either String [Constraint]
--
--solveConstraintsHelper [] = Right []
--
--solveConstraintsHelper ((Equals (BFresh n,_) (sub,_) _ ):cons) =
--  Right $ (substituteConstraintsB n sub cons)
--
--solveConstraintsHelper ((Equals (_,CFresh n) (_,sub) _ ):cons) =
--  Right $ (substituteConstraintsC n sub cons)
--
--solveConstraintsHelper ((Equals (sub,_) (BFresh n,_) _ ):cons) =
--  Right $ (substituteConstraintsB n sub cons)
--
--solveConstraintsHelper ((Equals (_,sub) (_,CFresh n) _ ):cons) =
--  Right $ (substituteConstraintsC n sub cons)
--
--solveConstraintsHelper ((Equals t1 t2 term):_) = Left (makeErrorMsg t1 t2 term)
--
--
--makeErrorMsg :: Type -> Type -> P.Term -> String
--makeErrorMsg expected actual term =
--  "Expected "++(show expected)++", but got "++(show actual)++" at "++(show (P.getPos term))++" in expression "++(show term)
--
--substituteConstraintsB :: Integer -> BaseType -> [Constraint]-> [Constraint]
--substituteConstraintsB n sub cons =map (substituteConstraintB n sub) cons
--
--substituteConstraintB :: Integer -> BaseType -> Constraint -> Constraint
--substituteConstraintB n sub (Equals t1 t2 term) = Equals (substituteB n sub t1) t2 term
--
--substituteB :: Integer -> BaseType -> Type -> Type
--substituteB n sub original@((BFresh n2), t2) = 
--  if n == n2
--    then (sub,t2) 
--    else original
--
--substituteB _ _ t2 = t2
--
--substituteConstraintsC :: Integer -> CardinalType -> [Constraint]-> [Constraint]
--substituteConstraintsC n sub cons = map (substituteConstraintC n sub) cons
--
--substituteConstraintC :: Integer -> CardinalType-> Constraint -> Constraint
--substituteConstraintC n sub (Equals t1 t2 term) = Equals t1 (substituteC n sub t2) term
--
--substituteC :: Integer -> CardinalType -> Type -> Type
--substituteC n sub original@(t1,(CFresh n2)) = 
--  if n == n2
--    then (t1,sub) 
--    else original
--
--substituteC _ _ t2 = t2
--
