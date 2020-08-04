{-# LANGUAGE GADTs #-}
module TypeChecker where

import qualified Calc.Parser as P
import qualified Data.Map as Map
import Control.Monad.State.Lazy
import Data.List
import Debug.Trace

--type TypedProgram = [(String,TypedTerm NoType)]

--data TConst = TConst
--data TNumPos = TNumPos
--data TLetterPos = TLetterPos
--data TDur = TDur
--data TMusic = TMusic

--class MyType a where
--  getType :: a 
--
--instance MyType TConst where
--  getType _ = TConst
--
--instance MyType TNumPos where
--  getType _ =TNumPos 
--
--instance MyType TLetterPos where
--  getType _ =TLetterPos 
--
--instance MyType TDur where
--  getType _ = TDur 
--
--instance MyType TMusic where
--  getType _ = TMusic 
--
--data TypedTerm a where
--   Constant :: Integer -> TypedTerm TConst
--   NumLeaf :: Integer -> TypedTerm TNumPos
--   LetterLeaf :: String -> TypedTerm TLetterPos
--   DurLeaf :: Integer -> TypedTerm TDur
--   Composition :: P.CompType -> TypedTerm a -> TypedTerm a -> TypedTerm a
--   Pattern :: TypedTerm a -> TypedTerm b -> TypedTerm (a,b)
--   Variable :: String -> a -> TypedTerm a
--   Context :: [String] ->  a -> TypedTerm a
--   Application :: TypedTerm (a,b) -> TypedTerm a -> TypedTerm b
--   Function :: a -> b -> TypedTerm (a,b)
--   Assignment :: String -> TypedTerm a -> TypedTerm NoType
----  deriving(Eq,Show)


data StateData = StateData {constraints :: [Constraint],
                            currentId :: Integer,
                            gamma :: Map.Map String TypedTerm
                           }


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
addConstraints cs = do state <- get
                       put (structAddConstraints cs state)
                       return ()

getId :: ConstraintM BaseType
getId = do state <- get 
           let (newState,n) = structGetId state
           put newState
           return $ BFresh n

updateGamma :: String -> TypedTerm -> ConstraintM ()
updateGamma var t = do state <- get
                       put (structUpdateGamma var t state)
                       return ()

lookUpGamma :: String -> ConstraintM TypedTerm
lookUpGamma var = do state <- get
                     case Map.lookup var (gamma state) of
                      Just t -> return t
                      Nothing -> return (ABase BNone,P.Variable (var,(0,0)))

--type Cardinality = Maybe Integer

--data ActualType = TConst  --fx octave value
--            | TNum 
--            | TDur --Model comp base types and possibly cardinality
--            | TLetter 
--            | TMusic
--            | TNote TypedTerm --Is it num or letter?
----            | TPos 
----            | TCardinal OpenType OpenType
--            | TComp OpenType OpenType 
--            | TSum [TypedTerm]
--            | TFun TypedTerm TypedTerm
--            | TList [TypedTerm]
--            | TPattern TypedTerm TypedTerm 
--            | TContext OpenType
--            | TAny -- Used as return value when the type doesn't matter
--            | TNone
--            | TSize Integer
--            | Fresh Integer -- OpenType variable
--  deriving(Show)


type TypedTerm = (ActualType,P.Term)

data BaseType = BConst  --fx octave value
            | BNum 
            | BDur --Model comp base types and possibly cardinality
            | BLetter 
            | BMusic
            | BAny -- Used as return value when the type doesn't matter
            | BNone
            | BSize Integer
            | BFresh Integer -- OpenType variable
            | BNote BaseType --Is it num or letter?
            | BComp BaseType BaseType 
  deriving(Show,Eq)

data ExpectedType = EBase BaseType
            | ESum [ExpectedType]
            | EFun ExpectedType ExpectedType
            | EList [ExpectedType]
            | EPattern ExpectedType ExpectedType
            | EContext ExpectedType
  deriving(Show,Eq)

data ActualType = ABase BaseType
            | ASum [TypedTerm]
            | AFun TypedTerm TypedTerm
            | AList [TypedTerm]
            | APattern TypedTerm TypedTerm
            | AContext ActualType

  deriving(Show,Eq)


baseMatch BAny _ = True
baseMatch _ BAny = True
baseMatch BNone _ = False
baseMatch _ BNone = False
baseMatch b1 b2 = b1 == b2


isCompBaseType t = foldl (\acc elem-> acc || (t==elem)) False [BDur,BNum,BLetter,BNote BAny,BMusic] 

typeMatch :: ExpectedType -> TypedTerm -> Bool
--base
typeMatch (EBase b1) ((ABase b2),_) = baseMatch b1 b2

typeMatch (ESum l1) ((ASum l2),_) = typeMatchLists l1 l2 

typeMatch (EFun l1 r1) ((AFun l2 r2),_) = typeMatch l1 l2 && typeMatch r1 r2

typeMatch (EList l1) ((AList l2),_) = typeMatchLists l1 l2

typeMatch (EPattern t1 t2) ((APattern t3 t4),_) = typeMatch t1 t3 && typeMatch t2 t4

--write bases here -TODO
typeMatchBases :: [BaseType] -> [BaseType] -> Bool
typeMatchBases expecteds actuals = typeMatchHelper expecteds actuals baseMatch

typeMatchLists :: [ExpectedType] -> [TypedTerm] -> Bool
typeMatchLists expecteds actuals = typeMatchHelper expecteds actuals typeMatch

typeMatchHelper expecteds actuals fun =
  length expecteds == length actuals && foldl (\acc (v1,v2)-> acc && (fun v1 v2) ) True (zip expecteds actuals)


--typesMatch (EBase $ BComp t1 _) t 
--  =isCompBaseType t
                 

--  (==) t1 (TComp t2 _) = t1 == t2 && isCompBaseType t1



--  (==) TConst TConst = True
--  (==) TNum TNum = True
--  (==) TDur TDur = True
--  (==) TLetter TLetter = True
--  (==) (TNote t1) (TNote t2) = t1 == t2
--  (==) TMusic TMusic = True
--  (==) (TFun t1 t2) (TFun t3 t4) = t1 == t3 && t2 == t4

--  (==) (TList l1) (TList l2) = l1 == l2
--  (==) (TSum l) t = elem t l
--
--  (==) (TPattern t1 t2) (TPattern t3 t4) = t1 == t3 && t2 == t4
--  (==) (TContext t1) (TContext t2) = t1 == t2
--  (==) TAny _ = True
--  (==) TNone _ = False
--  (==) _ TAny  = True
--  (==) _ TNone  = False
--
--  (==) (Fresh n1) (Fresh n2) = n1 == n2
--  (==) (TSize n1) (TSize n2) = n1 == n2
--  (==) _ _ = False



data Constraint = Equals ExpectedType TypedTerm
  deriving(Eq)

instance Show Constraint where
  show (Equals t1 (t2,_)) = (show t1) ++ " = " ++ (show t2)

generateConstraints :: P.Program -> [Constraint]
generateConstraints program = 
  let state = StateData [] 0 Map.empty
  in let (_,newState) = runState (genConstraints program) state 
     in constraints newState

genConstraints :: P.Program -> ConstraintM ()
genConstraints program = do mapM_ genConstraint program
                            main_type <-  lookUpGamma "main" --Later check main is present
                            addConstraints [Equals (EBase (BComp BMusic BAny)) main_type]
                              

genConstraint :: P.Assignment -> ConstraintM ()

genConstraint (P.Assignment ("main",_) term) = do
  term_t <- genConstraintT term
  updateGamma "main" term_t
  addConstraints [Equals (EBase (BComp BMusic BAny)) term_t]

genConstraint (P.Assignment (var,_) term) = do
  term_t <- genConstraintT term
  updateGamma var term_t

actualToExpectedType :: ActualType -> ExpectedType
actualToExpectedType (ABase t) = EBase t
actualToExpectedType (ASum ts) = listConversion ESum ts
actualToExpectedType (AFun t1 t2) = binaryConversion EFun (fst t1) (fst t2)
actualToExpectedType (AList ts) = listConversion EList ts
actualToExpectedType (APattern t1 t2) = binaryConversion EPattern (fst t1) (fst t2)
actualToExpectedType (AContext t) = EContext $ actualToExpectedType t

actualToBase :: ActualType -> BaseType
actualToBase (ABase t) =t
actualToBase _ = BNone



listConversion con ts = 
  let ts_first = map fst ts
  in let actual_ts = map actualToExpectedType ts_first
     in con actual_ts

binaryConversion con t1 t2 = 
  let actual_t1 = actualToExpectedType t1
      actual_t2 = actualToExpectedType t1
  in con actual_t1 actual_t2




genConstraintT :: P.Term -> ConstraintM TypedTerm 

genConstraintT term@(P.Num _) = do id <- getId  --Not Pos because might be Const genConstraintT (P.Dur _) = return $ TDur
                                   return (ABase id,term)
genConstraintT term@(P.Dur _) = return $ (ABase BDur,term)
genConstraintT term@(P.Letter _) = return $ (ABase BLetter,term)
genConstraintT term@(P.FlatList ((_,terms,n),_)) = do
  types <- mapM genConstraintT terms
  case types of
    [] -> return (ABase BNone,term)  -- Shouldn't be possible so allow any type, and expect some other constraint will fail
    ((first,p):rest) -> do addConstraints (map (\t -> Equals (actualToExpectedType first) t) rest) --possibly some helper functions here to extract base
--                           addConstraints [Equals (TComp TAny TAny) (first,p)]   --It's a Composotion, but comp of what? 
                           return $ (ABase (BComp (actualToBase first) (BSize n)),term)  --Combine base type from first and cardinality from argument

genConstraintT term@(P.Pattern ((t1,t2),_)) = 
  let validTypes = [EBase BDur,EBase BNum,EBase BLetter]
  in do t1_type <- genConstraintT t1
        t2_type <- genConstraintT t2
        addConstraints [Equals (EBase (BComp BAny BAny)) t1_type, Equals (EBase (BComp BAny BAny)) t2_type] --genConstraintT should return cardinal for FlatList
        return $ (APattern t1_type t2_type,term)

genConstraintT term@(P.Variable (str,_)) = lookUpGamma str --Var should do something else

genConstraintT term@(P.Context (pairs,_)) = 
  let (labels,terms) = unzip pairs
  in let label_types = map labelType labels
     in do types <- mapM genConstraintT terms
           let newConstraints = equalityConstraintsFromList label_types types --TODO
           addConstraints newConstraints
           case sort pairs of
             [(P.Octave,t1),(P.Key,t2)] -> return $ (AContext (AList [(ABase BNum,t1),(ABase BLetter,t2)]),term)  --matches any Pos
             [(P.Octave,t)] -> return $ (AContext (ABase BLetter),term) --matches any Pos


      
genConstraintT term@(P.Function ("transform",_)) = undefined
    
genConstraintT term@(P.Application ((P.Function (name,_),terms),_)) =  ---TODO return here
  do t1 <- expectedFunctionType name
     ts <- mapM genConstraintT terms
     id <- getId
     addConstraints [Equals t1 ((AFun (AList ts) ((ABase id),undefined),term))]
     return (id,term)
  
expectedFunctionType "toNotes" =  
  do id1 <- getId
     id2 <- getId
     let t1 = EBase $ EComp BDur id1
         t2 = EBase $ EComp id2 id1
         t3 = EBase $ ENote id2
     return $ EFun (EList [t1,t2]) t3

expectedFunctionType "toMusic" = 
  do id <- getId
     let t1 = BNote id
         t2 = EContext (EBase id)
         t3 = BComp BMusic BAny
     return $ EFun (EList [t1,t2]) t3

expectedFunctionType _ = return $ EBase BNone

splitFunConstraint (Equals (EFun (EList l1) r1) actual@((AFun (AList l2) r2),p)) = 
  if length l1 == length l2
    then let cs = map (\(t1,t2) -> Equals t1 t2) (zip l1 l2)
             ret_con = Equals r1 r2
         in ret_con:cs
    else [Equals (EBase BNone) (ABase BNone,snd actual)]
    
splitFunConstraint c = [c]

equalityConstraintsFromList :: [ExpectedType] -> [TypedTerm] -> [Constraint]
equalityConstraintsFromList expected actual = 
  if length expected == length actual
    then  map (\(ex,ac)-> Equals ex ac) (zip expected actual)
    else []


labelType :: P.Label -> ExpectedType
labelType P.Key = EBase BLetter
labelType P.Octave = EBase BConst

--indexPattern :: Integer ->  TypedTerm -> TypedTerm
--indexPattern 0 (TPattern t1 _ ) = t1
--indexPattern 1 (TPattern _ t2 ) = t2
--indexPattern _ t = t 

---------------------------------------------------------------------------------
type SolveResult = Either String [Constraint]

substitute :: Integer -> ExpectedType -> [Constraint] -> [Constraint]
substitute n t cons = map (\con -> substituteConstraint n con t) cons


substituteConstraint :: Integer -> Constraint -> ExpectedType -> Constraint 
substituteConstraint n (Equals t1 t2) t= 
  Equals (substituteExpectedType n t1 t) (substituteActualType n t2 t)

substituteExpectedType :: Integer -> ExpectedType-> ExpectedType-> TypedTerm
substituteExpectedType n oldType newType =
  case oldType of
    EBase (BFresh n2) -> 
                if n == n2
                  then newType
                  else oldType
    EBase (BNote t) -> EBase $ BNote (substituteExpectedType n t newType)
    BComp t nc -> EBase $ BComp (substituteExpectedType n t newType) (substituteExpectedType n nc newType)
    EFun (EList l) r -> let l_sub = map (\t -> substituteExpectedType n t newType) l
                            r_sub = substituteExpectedType n r newType
                        in EFun (EList l_sub) r_sub
    _ -> oldType



substituteActualType :: Integer -> ActualType -> ExpectedType-> TypedTerm
substituteActualType n oldType newType =
  case oldType of
    (ABase (BFresh n2),p) -> 
                if n == n2
                  then (newType,p)
                  else oldType
    (ABase (BNote t),p) -> (ABase $ BNote (substituteActualType n t newType),p)
    (ABase (BComp t nc),p) -> (ABase $ BComp (substituteActualType n t newType) 
                                             (substituteActualType n nc newType),p)
    (EFun (EList l) r,p) -> let l_sub = map (\t -> substituteActualType n t newType) l
                                r_sub = substituteActualType n r newType
                        in AFun (AList l_sub) r_sub
    _ -> oldType






--substituteType :: Integer -> ExpectedType-> ExpectedType-> ExpectedType
--substituteType n oldType newType = 
--  case oldType of
--    EBase (BFresh n2) -> 
--                if n == n2
--                  then newType
--                  else oldType
--    EBase (BNote t) -> BNote (substituteType n t newType)
--    BComp t nc -> BComp (substituteType n t newType) (substituteType n nc newType)
--    EFun (EList l) r -> let l_sub = map (\t -> substituteType n t newType) l
--                            r_sub = substituteType n r newType
--                        in EFun (EList l_sub) r_sub
--    _ -> oldType


solve constraints = do 
  one_way_solved <- solveConstraints constraints
  two_way_solved <- solveConstraints (reverse one_way_solved)
  let potential_solution = reverse two_way_solved
  if constraints == potential_solution
    then return potential_solution
    else solve potential_solution


solveConstraints :: [Constraint] -> SolveResult
solveConstraints [] = return []
solveConstraints (first@(Equals expected actual):rest) = --How to handle Equals (expected:fresh) (acftual:t)?
  if expected == fst actual
    then solveConstraints rest
    else solveConstraintsHelper expected actual rest


--Recursive
solveConstraintsHelper (EBase (BNote t1)) ((EBase (BNote t2)),p) rest =
  solveConstraints ((Equals t1 (t2,p)):rest) --handle recursive types

solveConstraintsHelper (EContext t1) (EContext t2,p) rest =
  solveConstraints (Equals t1 (t2,p):rest)

--Split function
solveConstraintsHelper expected@(EFun (EList l1) r1) actual@((EFun (EList l2) r2),_) rest =
  let new_cons = splitFunConstraint (Equals expected actual)
  in solveConstraints (new_cons++rest)

--Substitute
solveConstraintsHelper (EBase (BFresh n1)) t2 rest =
  let subbed = substitute n1 (fst t2) rest
      first = Equals (Fresh n1) t2
  in do solved_rest <- solveConstraints subbed
        return $ first:solved_rest

solveConstraintsHelper t1 (EBase (BFresh n2),p) rest =
  let subbed = substitute n2 t1 rest
      first = Equals t1 (Fresh n2,p)
  in do solved_rest <- (solveConstraints subbed)
        return $ first:solved_rest

solveConstraintsHelper expected@(EBase (BComp t11 t12)) actual@((EBase (BComp t21 t22)),p) rest =
  let c1 = Equals t11 (t21,p) 
      c2 = Equals t12 (t22,p) 
  in solveConstraints $ c1:c2:rest

solveConstraintsHelper expected actual _ = Left (makeErrorMsg expected actual)

makeErrorMsg expected (actual,t)=
  "Expected "++(show expected)++", but got "++(show actual)++" at "++(show (P.getPos t))++" in expression "++(show t)
