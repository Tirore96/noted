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

getId :: ConstraintM Integer
getId = do state <- get 
           let (newState,n) = structGetId state
           put newState
           return n

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

--type LogicalType = BAny -- Used as return value when the type doesn't matter
--            | BNone

data BaseKind = KNum 
            | KDur --Model comp base types and possibly cardinality
            | KLetter 
            | KFresh Integer
            | KAny
            | KNone
  deriving(Show,Eq)

data SizeType = SSize Integer
            | SFresh Integer
            | SAny
            | SNone

  deriving(Show,Eq)

data BaseType = BConst
            | BAny
            | BNone
            | BSize Integer
            | BNote BaseKind
            | BMusic
            | BComp BaseKind SizeType
            | BFresh Integer -- OpenType variable
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


--isCompBaseType t = t == BDur || [BDur,BNum,BLetter,BNote BAny,BMusic] 
--                   t == BNum ||
--                   t == BLetter ||
--                   t == BNote BAny ||

typeMatchK :: BaseKind -> (BaseKind,P.Term) -> Bool
typeMatchK KAny _ = True
typeMatchK _ (KAny,_) = True
typeMatchK KNone _ = False
typeMatchK _ (KNone,_) = False
typeMatchK k1 (k2,_) = k1 == k2

typeMatch :: ExpectedType -> TypedTerm -> Bool
--base
typeMatch (EBase b1) ((ABase b2),_) = baseMatch b1 b2

typeMatch (ESum l1) ((ASum l2),_) = typeMatchLists l1 l2 

typeMatch (EFun l1 r1) ((AFun l2 r2),_) = typeMatch l1 l2 && typeMatch r1 r2

typeMatch (EList l1) ((AList l2),_) = typeMatchLists l1 l2

typeMatch (EPattern t1 t2) ((APattern t3 t4),_) = typeMatch t1 t3 && typeMatch t2 t4

typeMatch t1 t2 = False

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
                            addConstraints [Equals (EBase BMusic) main_type]
                              

genConstraint :: P.Assignment -> ConstraintM ()

genConstraint (P.Assignment ("main",_) term) = do
  term_t <- genConstraintT term
  updateGamma "main" term_t
  addConstraints [Equals (EBase BMusic) term_t]

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

actualToKind :: ActualType -> BaseKind
actualToKind (ABase (BComp k _)) = k
actualToKind _ = KNone



listConversion con ts = 
  let ts_first = map fst ts
  in let actual_ts = map actualToExpectedType ts_first
     in con actual_ts

binaryConversion con t1 t2 = 
  let actual_t1 = actualToExpectedType t1
      actual_t2 = actualToExpectedType t1
  in con actual_t1 actual_t2


kindToEType:: BaseKind -> ExpectedType
kindToEType k = EBase (BComp k SAny)

kindToAType:: BaseKind -> ActualType
kindToAType k = ABase (BComp k SAny)


genConstraintT :: P.Term -> ConstraintM TypedTerm 

genConstraintT term@(P.Num _) = do id <- getId  --Not Pos because might be Const genConstraintT (P.Dur _) = return $ TDur
                                   return (ABase (BFresh id),term)
genConstraintT term@(P.Dur _) = return $ (ABase $ BComp KDur (SSize 1),term)
genConstraintT term@(P.Letter _) = return $ (ABase $ BComp KLetter (SSize 1),term)
genConstraintT term@(P.FlatList ((_,terms,n),_)) = do
  types <- mapM genConstraintT terms
  case types of
    [] -> return (ABase BNone,term)  -- Shouldn't be possible so allow any type, and expect some other constraint will fail
    _ -> do id1 <- getId
            let expected_member = EBase $ BComp (KFresh id1) SAny
            addConstraints (map (\t -> Equals expected_member t) types)
            return $ (ABase (BComp (KFresh id1) (SSize n)),term)  --Combine base type from first and cardinality from argument

genConstraintT term@(P.Pattern ((t1,t2),_)) = 
  let validTypes = map kindToEType [KDur,KNum,KLetter]
  in do t1_type <- genConstraintT t1
        t2_type <- genConstraintT t2
        addConstraints [Equals (EBase (BComp KAny SAny)) t1_type, Equals (EBase (BComp KAny SAny)) t2_type] --genConstraintT should return cardinal for FlatList
        return $ (APattern t1_type t2_type,term)

genConstraintT term@(P.Variable (str,_)) = lookUpGamma str --Var should do something else

genConstraintT term@(P.Context (pairs,_)) = 
  let (labels,terms) = unzip pairs
  in let label_types = map labelType labels
     in do types <- mapM genConstraintT terms
           let newConstraints = equalityConstraintsFromList label_types types --TODO
           addConstraints newConstraints
           case sort pairs of
             [(P.Octave,t1),(P.Key,t2)] -> return $ (AContext (AList [(kindToAType KNum,t1),(kindToAType KLetter,t2)]),term)  --matches any Pos
             [(P.Octave,t)] -> return $ (AContext (kindToAType KLetter),term) --matches any Pos

genConstraintT term@(P.ArgList (l,_)) = 
  do ts <- mapM genConstraintT l 
     return (AList ts,term)

      
genConstraintT term@(P.Function ("transform",_)) = undefined
    
genConstraintT term@(P.Application ((P.Function (name,_),argsTerm),_)) =  ---TODO return here
  do t1 <- expectedFunctionType name
     ts <- genConstraintT argsTerm
     id <- getId
     let typedTerm= (AFun ts ((ABase (BFresh id)),term),term)
     addConstraints [Equals t1 typedTerm]
     return (ABase (BFresh id),term)
  
expectedFunctionType "toNotes" =  
  do id1 <- getId
     id2 <- getId
     let t1 = EBase $ BComp KDur (SFresh id1)
         t2 = EBase $ BComp (KFresh id2) (SFresh id1)
         t3 = EBase $ BNote (KFresh id2)
     return $ EFun (EList [t1,t2]) t3

expectedFunctionType "toMusic" = 
  do id <- getId
     let t1 = EBase $ BNote (KFresh id)
         t2 = EContext (EBase (BFresh id))
         t3 = EBase BMusic 
     return $ EFun (EList [t1,t2]) t3

expectedFunctionType _ = return $ EBase BNone

splitFunConstraint (Equals (EFun (EList l1) r1) actual@((AFun (AList l2,_) r2),p)) = 
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
labelType P.Key = EBase $ BComp KLetter (SSize 1)
labelType P.Octave = EBase BConst

--indexPattern :: Integer ->  TypedTerm -> TypedTerm
--indexPattern 0 (TPattern t1 _ ) = t1
--indexPattern 1 (TPattern _ t2 ) = t2
--indexPattern _ t = t 

---------------------------------------------------------------------------------
type SolveResult = Either String [Constraint]

--match integer and substitution value
data SubstitutionPair = KindSub Integer BaseKind | SizeSub Integer SizeType |  Sub Integer ExpectedType

substitute :: SubstitutionPair -> [Constraint] -> [Constraint]
substitute sp cons= map (\con -> substituteConstraint sp con ) cons


substituteConstraint :: SubstitutionPair-> Constraint ->  Constraint 
substituteConstraint sp (Equals t1 (t2,p))= 
  Equals (substituteExpectedType sp t1) (substituteActualType sp t2,p)

--substituteConstraint (KindSub n k) (KEquals k1 (k2,p))= 
--  KEquals (substituteKind n k k1) (substituteKind n k k2,p)

substituteConstraint (Sub _ _) retval = retval

substituteSize :: Integer -> SizeType -> SizeType -> SizeType
substituteSize n newSize oldSize@(SFresh n2) = --substituteBase handles BaseTypes
  if n==n2 then newSize else oldSize

substituteSize _ oldSize _ = oldSize

substituteKind :: Integer -> BaseKind -> BaseKind -> BaseKind
substituteKind n newKind oldKind@(KFresh n2) = --substituteBase handles BaseTypes
  if n==n2 then newKind else oldKind

substituteKind _ oldBase _ = oldBase

substituteBase :: Integer -> BaseType -> BaseType -> BaseType
substituteBase n newBase oldBase@(BFresh n2) =
  if n == n2 then newBase else oldBase

substituteBase _ oldBase _ = oldBase

binarySubstitution sp con t1 t2 fun = 
  let t1_new = fun sp t1 
      t2_new = fun sp t2
  in con t1 t2

genericSubstitutionHelper :: SubstitutionPair -> BaseType -> BaseType

genericSubstitutionHelper (Sub n (EBase bNew)) b  = substituteBase n bNew b 

genericSubstitutionHelper (KindSub n k) (BComp k1@(KFresh n2) b2)  = 
  let newKind = if n == n2 then k else k1
  in BComp newKind b2

genericSubstitutionHelper (KindSub n k) (BNote k1@(KFresh n2))  = 
  let newKind = if n == n2 then k else k1
  in BNote newKind

genericSubstitutionHelper (SizeSub n s) (BComp k s1@(SFresh n2))  = 
  let newSize = if n == n2 then s else s1
  in BComp k newSize


genericSubstitutionHelper _ b  = b


substituteExpectedType :: SubstitutionPair -> ExpectedType-> ExpectedType
substituteExpectedType sp oldType =
  case oldType of
    EBase b -> let newB = genericSubstitutionHelper sp b
               in EBase newB
                 
--    EBase (BNote t) -> EBase $ BNote (substituteExpectedType n t newType)
    ESum l ->  let l_new = map (\t -> substituteExpectedType sp t) l
               in ESum l_new
    EFun t1 t2 -> binarySubstitution sp EFun t1 t2 substituteExpectedType 
--let t1_new = substituteExpectedType n t1 newType
--                      t2_new = substituteExpectedType n t2 newType
--                  in EFun t1 t2
    EList l -> let l_new = map (\t -> substituteExpectedType sp t ) l
               in EList l_new
    EPattern t1 t2 -> binarySubstitution sp EPattern t1 t2 substituteExpectedType

--let t1_new = substituteExpectedType n t1 newType
--                          t2_new = substituteExpectedType n t2 newType
--                      in EPattern t1_new t2_new
    EContext t -> let t_new = substituteExpectedType sp t 
                  in EContext t_new



substituteActualType :: SubstitutionPair -> ActualType -> ActualType
substituteActualType sp oldType =
  case oldType of
    ABase b -> let newB = genericSubstitutionHelper sp b
               in ABase newB
    ASum l ->  let l_new = map (\(t,p) -> (substituteActualType sp t,p)) l
               in ASum l_new
    AFun (t1,p1) (t2,p2) -> let t1_new = substituteActualType sp t1 
                                t2_new = substituteActualType sp t2 
                            in AFun (t1_new,p1) (t2_new,p2)
    AList l -> let l_new = map (\(t,p) -> (substituteActualType sp t ,p)) l
               in AList l_new
    APattern (t1,p1) (t2,p2) -> let t1_new = substituteActualType sp t1 
                                    t2_new = substituteActualType sp t2 
                                in APattern (t1_new,p1) (t2_new,p2)
    AContext t -> let t_new = substituteActualType sp t 
                  in AContext t_new






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
  if typeMatch expected actual 
    then solveConstraints rest
    else solveConstraintsHelper expected actual rest

--solveConstraints (first@(KEquals expected actual):rest) = --How to handle Equals (expected:fresh) (acftual:t)?
--  if typeMatchK expected actual 
--    then solveConstraints rest
--    else solveConstraintsHelperK expected actual rest
--
--solveConstraintsHelperK expected@(KFresh n) actual rest =
--  let sp = KindSub n (fst actual)
--      first = KEquals expected actual
--  in let subbed = substitute sp rest
--     in do solved <- solveConstraints subbed
--           return $ first:solved
--
--solveConstraintsHelperK expected actual@(KFresh n,_) rest =
--  let sp = KindSub n expected
--      first = KEquals expected actual 
--  in let subbed = substitute sp rest
--     in do solved <- solveConstraints subbed
--           return $ first:solved
--
--solveConstraintsHelperK expected actual _ = Left (makeErrorMsg expected actual)

--solveConstraintsHelper expected@(KFresh n1) (k2,p) rest =


--Recursive

substituteSolveE sp expected actual rest =
  let subbed = substitute sp rest
      first = Equals expected actual
  in do solved_rest <- solveConstraints subbed
        return $ first:solved_rest

substituteSolveE _ _ _ _ = undefined

substituteSolveA sp expected actual rest =
  let subbed = substitute sp rest
      first = Equals expected actual
  in do solved_rest <- solveConstraints subbed
        return $ first:solved_rest

substituteSolveA _ _ _ _  = undefined
solveConstraintsHelper (EContext t1) (AContext t2,p) rest =
  solveConstraints (Equals t1 (t2,p):rest)

--Split function
solveConstraintsHelper expected@(EFun (EList l1) r1) actual@((AFun (AList l2,_) r2),_) rest =
  let new_cons = splitFunConstraint (Equals expected actual)
  in solveConstraints (new_cons++rest)

--Substitute
solveConstraintsHelper expected@(EBase (BFresh n1)) actual rest =
  let t2_expected = actualToExpectedType (fst actual)
      sp = Sub n1 t2_expected
  in substituteSolveE sp expected actual rest

solveConstraintsHelper expected actual@(ABase (BFresh n2),p) rest =
  let sp = Sub n2 expected
  in substituteSolveA sp expected actual rest

--  let sp = Sub n2 t1
--  in let subbed = substitute sp rest
--         first = Equals t1 actual
--     in do solved_rest <- (solveConstraints subbed)
--           return $ first:solved_rest

solveConstraintsHelper expected@(EBase (BNote (KFresh n1))) actual@(ABase (BNote k), _) rest =
  let sp = KindSub n1 k
  in substituteSolveE sp expected actual rest

solveConstraintsHelper expected@(EBase (BNote k)) actual@(ABase (BNote (KFresh n1)),  _) rest =
  let sp = KindSub n1 k
  in substituteSolveA sp expected actual rest

solveConstraintsHelper expected@(EBase (BComp (KFresh n1) t12)) actual@((ABase (BComp k t22)),p) rest =
  let sp = KindSub n1 k
  in substituteSolveE sp expected actual rest

solveConstraintsHelper expected@(EBase (BComp k t12)) actual@((ABase (BComp (KFresh n1) t22)),p) rest =
  let sp = KindSub n1 k
  in substituteSolveA sp expected actual rest

solveConstraintsHelper expected@(EBase (BComp k1 (SFresh n1))) actual@((ABase (BComp k2 s)),p) rest =
  let sp = SizeSub n1 s 
  in substituteSolveE sp expected actual rest

solveConstraintsHelper expected@(EBase (BComp k1 s)) actual@((ABase (BComp k2 (SFresh n1))),p) rest =
  let sp = SizeSub n1 s
  in substituteSolveA sp expected actual rest

solveConstraintsHelper expected actual _ = Left (makeErrorMsg expected actual)

makeErrorMsg expected (actual,t)=
  "Expected "++(show expected)++", but got "++(show actual)++" at "++(show (P.getPos t))++" in expression "++(show t)
