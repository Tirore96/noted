module TypeChecker where
--Possibly type check for repeating labels. ie {quantization=10;quantization=20} shouldn't be allowed

import Data.Map as Map
import Control.Monad 
import Data.Set as Set

import Calc.Lexer (AlexPosn)
--Temporary data structure for AbSyn
--data Exp = Let Variable Assignable Exp | Variable | In Composition Context 
data Exp = In Composition Context -- | In Composition Context Exp
  deriving(Eq,Show)

type Pos = (Int,Int)

data Composition = NoteN String Integer Pos | Dotted Composition | Concatted Composition Composition
  deriving(Eq,Show)

type Context = Map CtxLabel CtxVal

data CtxLabel = Quantization Pos
  deriving(Eq,Show,Ord)

data CtxVal = Int AlexPosn
  deriving(Eq,Show)


--Changes: Note, CtxVal, CtxLabel


--Data structure for types
data Type = TMusic | TComposition | TInt | TKey | TContext
  deriving(Eq,Show)

data TComposition =  TExp TDirection TSymbol TPosition
  deriving(Eq,Show)
data TDirection = TSerial | TParallel | TNote
  deriving(Show)
data TSymbol = TByLetter | TByNum
  deriving(Eq,Show)
data TPosition = TPositioned | TNotPositioned
  deriving(Eq,Show)

type TContext = Set CtxLabel


instance Eq TDirection where
  TParallel  == TParallel = True
  TParallel == TSerial = False
  TParallel == TNote = True
  TSerial == TParallel = False
  TSerial == TSerial = True
  TSerial == TNote = True
  TNote  == TParallel = True
  TNote  == TSerial = True
  TNote  == TNote = True


--Data structure for Constraints
--data ConstraintMember = T Type | U Int
--data Constraint = Constraint ConstraintMember ConstraintMember 

--newConstraint :: ConstraintMember -> ConstraintMember -> Constraint
--newConstraint  (T t1) (T t2) = TTCon t1 t2
--newConstraint  (T t) (U u) = TUCon t u
--newConstraint  (U u1) (U u2) = UUCon u1 u2
--newConstraint a b = newConstraint b a



--data Variable = String


--data StateData = StateData {gamma :: Map Variable ConstraintMember,constraints :: [Constraint]}
--data ReadData = Exp | Composition | Context | CtxAssignment
type StateData = Map Type Type

newtype SEM a =  SEM {runSEM :: StateData -> Either String (a,StateData) } 

--emptyStateData :: StateData
--emptyStateData = StateData {gamma=empty,constraints=[]}

instance Monad SEM where
  return a = SEM (\stateData -> Right (a,stateData) )
  m >>= f = SEM (\stateData -> do (a1,s1) <- (runSEM m) stateData
                                  (a2,s2) <- runSEM (f a1) s1
			          Right (a2,s2) 
		)

instance Functor SEM where 
  fmap = liftM

instance Applicative SEM where
  pure = return;(<*>) = ap

asSerial :: TComposition -> TComposition
asSerial (TExp _ symbol position) = TExp TSerial symbol position


failM :: String -> SEM ()
failM str =SEM (\state -> Left str)


assertNotParallelComp :: TComposition -> String -> SEM ()
assertNotParallelComp (TExp TParallel _ _) s = failM ("Parallel compositions are not allowed here"++s)
assertNotParallelComp _ _ = return ()
assertEqualComps :: TComposition -> TComposition -> String -_> SEM ()
assertEqualComps c1 c2 s = if c1 == c2 then return () else failM ("Compositions are not equal"++s)


typeCheckExp :: Exp -> SEM ()
typeCheckExp (In comp con) = do compT <- typeCheckComp comp
				conT <- typeCheckCon con
			        let depL = dependencyLabels compT
				if depL `isSubsetOf`conT 
				  then return ()
			        else 
				    let violatingL = Set.filter (\elem -> not $ Set.member elem conT) depL
				    in failM ("Missing labels: "++(show violatingL))

dependencyLabels :: TComposition -> TContext
dependencyLabels (TExp _ TByLetter TNotPositioned) = Set.fromList [Quantization]
dependencyLabels (TExp _ TByNum TNotPositioned) = Set.fromList [Quantization]
dependencyLabels (TExp _ TByLetter TPositioned ) = Set.fromList [Quantization]
dependencyLabels (TExp _ TByNum TPositioned) = Set.fromList [Quantization]

showCodeWithPos :: Composition -> String
showCodeWithPos comp = (showCode comp)++" at: "++(findFirstPos comp)

showCode :: Composition -> String
showCode (NoteN n s p) = s++(show n)
showCode (Dotted comp) = (showCode comp)++"."
showCode (Concatted c1 c2) = (showCode c1)++(showCode c2)

findFirstPos :: Composition -> Pos
findFirstPos (NoteN n s p) = p
findFirstPos (Dotted comp) = comp
findFirstPos (Concatted c1 c2) = findFirstPos c1


typeCheckComp :: Composition -> SEM TComposition
typeCheckComp (NoteN _ _ _) = return $ TExp TNote TByLetter TPositioned

typeCheckComp (Dotted comp) = do compT <- typeCheckComp comp
				 assertNotParallelComp compT (showCodeWithPos comp)
				 return $ asSerial compT

typeCheckComp (Concatted c1 c2) = do c1T <- typeCheckComp c1
				     assertNotParallelComp c1T (showCodeWithPos c1)
				     c2T <- typeCheckComp c2
			     	     assertNotParallelComp c2T (showCodeWithPos c2)
				     assertEqualComps c1T c2T (showCodeWithPos (Concatted c1 c2))
				     return $ asSerial c1T

typeCheckCon :: Context -> SEM TContext
typeCheckCon con = let labels = [Quantization]
		       presentLabels = Prelude.filter (\label -> Map.member label con) labels
		   in do return $ Set.fromList presentLabels
