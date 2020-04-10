module TypeChecker where
--Possibly type check for repeating labels. ie {quantization=10;quantization=20} shouldn't be allowed

import Data.Map as Map
import Control.Monad 
import Data.Set as Set

import Calc.Lexer (AlexPosn)
import Calc.Parser

--Temporary data structure for AbSyn
--data Exp = In Composition Context -- | In Composition Context Exp
--  deriving(Eq,Show)
--
--type Pos = (Int,Int)
--
--data Composition = NoteN String Integer Pos | Dotted Composition | Concatted Composition Composition
--  deriving(Eq,Show)
--
--type Context = Map CtxLabel CtxVal
--
--data CtxLabel = Quantization Pos
--  deriving(Eq,Show,Ord)
--
--data CtxVal = Int AlexPosn
--  deriving(Eq,Show)


--Changes: Note, CtxVal, CtxLabel
data Type = TInt
           | TKey
           | TSignature
           | TMusicExp TSymbol TPosition
           | TContext (Set Label)
           | TMusic
  deriving(Eq,Show)
--Data structure for types
--data Type = TMusic | TComposition | TInt | TKey | TContext
--
data TSymbol = ByLetter | ByNum
  deriving(Eq,Show)
data TPosition = Positioned | NotPositioned
  deriving(Eq,Show)
--
--
--data TComposition =  TExp TDirection TSymbol TPosition
--  deriving(Eq,Show)
--data TDirection = TSerial | TParallel | TNote
--  deriving(Show)


--instance Eq TDirection where
--  TParallel  == TParallel = True
--  TParallel == TSerial = False
--  TParallel == TNote = True
--  TSerial == TParallel = False
--  TSerial == TSerial = True
--  TSerial == TNote = True
--  TNote  == TParallel = True
--  TNote  == TSerial = True
--  TNote  == TNote = True


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
type StateData = Map String Type

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

updateState :: String -> Type ->  SEM ()
updateState str t = SEM (\s -> Right ((),Map.insert str t s))

lookupVar :: String -> SEM Type
lookupVar str = SEM (\s -> case Map.lookup str s of 
                            Just t -> Right (t,s)
                            Nothing -> Left "bad variable lookup")

assertSameType :: Type -> Type -> SEM ()
assertSameType t1 t2 = if t1 == t2
                         then return ()
                         else failM "the types don't work"


typeCheck :: Exp -> Either String ((),StateData)
typeCheck exp = runSEM (typeCheckExp exp) Map.empty
typeCheckExp :: Exp -> SEM ()
typeCheckExp (Assign (str,pos) term exp) = do termT <- typeCheckTerm term
                                              updateState str termT
                                              typeCheckExp exp
typeCheckExp (Term term) = do termT <- typeCheckTerm term
                              assertSameType termT TMusic

typeCheckTerm :: Term -> SEM Type
typeCheckTerm (Num _) = return $ TMusicExp ByNum Positioned
typeCheckTerm (Note _) = return $ TMusicExp ByLetter NotPositioned
typeCheckTerm (NoteN _) = return $ TMusicExp ByLetter Positioned
typeCheckTerm (Dotted term) = typeCheckTerm term
typeCheckTerm (Concatted term1 term2) = do term1T <- typeCheckTerm term1
                                           term2T <- typeCheckTerm term2
                                           assertSameType term1T term2T
                                           return term1T
typeCheckTerm (Commaed term1 term2)= do term1T <- typeCheckTerm term1
                                        term2T <- typeCheckTerm term2
                                        assertSameType term1T term2T
                                        return term1T
typeCheckTerm (Variable (str,_)) = lookupVar str
typeCheckTerm (Context labelPairs) = do mapM_ typeCheckLabelPair labelPairs 
                                        let labels = fst $ unzip labelPairs
                                        return $ TContext (Set.fromList labels) --Set.fromList



typeCheckTerm (Application term1 term2) = do term1T <- typeCheckTerm term1
                                             term2T <- typeCheckTerm term2
                                             assertApplicationSatisfied term1T term2T
typeCheckLabelPairHelper (Bars,val) expectedT  = do valT <- typeCheckCtxVal val
                                                    assertSameType valT expectedT
typeCheckCtxVal (CNum _ ) = return TInt
typeCheckCtxVal (CNote _ ) = return TKey
typeCheckCtxVal (Signature _ ) = return TSignature



typeCheckLabelPair (Bars,val) = typeCheckLabelPairHelper (Bars,val) TInt
typeCheckLabelPair (Key,val) = typeCheckLabelPairHelper (Bars,val) TKey
typeCheckLabelPair (Time ,val) = typeCheckLabelPairHelper (Bars,val) TSignature
typeCheckLabelPair (OctavePos,val) = typeCheckLabelPairHelper (Bars,val) TInt


assertTCtxContains (TContext tCtx) label = if Set.member label tCtx
                                    then return ()
                                    else failM "something went wrong"

assertTCtxContains _ _ = failM "assertTCtxContains applied on non-context"


assertApplicationSatisfied (TMusicExp ByLetter Positioned) term2T = return TMusic 
assertApplicationSatisfied (TMusicExp ByNum Positioned) term2T = do assertTCtxContains term2T Key
                                                                    return TMusic
assertApplicationSatisfied (TMusicExp ByLetter NotPositioned) term2T = do assertTCtxContains term2T OctavePos
                                                                          return TMusic
assertApplicationSatisfied (TMusicExp ByNum NotPositioned) term2T = do assertTCtxContains term2T Key
                                                                       assertTCtxContains term2T OctavePos
                                                                       return TMusic

--asSerial :: TComposition -> TComposition
--asSerial (TExp _ symbol position) = TExp TSerial symbol position


failM :: String -> SEM ()
failM str =SEM (\state -> Left str)


--assertNotParallelComp :: TComposition -> String -> SEM ()
--assertNotParallelComp (TExp TParallel _ _) s = failM ("Parallel compositions are not allowed here"++s)
--assertNotParallelComp _ _ = return ()
--assertEqualComps :: TComposition -> TComposition -> String -> SEM ()
--assertEqualComps c1 c2 s = if c1 == c2 then return () else failM ("Compositions are not equal"++s)
--
--
--typeCheck :: Exp -> Either String ((),StateData)
--typeCheck exp = runSEM (typeCheckExp exp) Map.empty
--typeCheckExp :: Exp -> SEM ()
--typeCheckExp (In comp con) = do compT <- typeCheckComp comp
--				conT <- typeCheckCon con
--			        let depL = dependencyLabels compT
--				if depL `isSubsetOf`conT 
--				  then return ()
--			        else 
--				    let violatingL = Set.filter (\elem -> not $ Set.member elem conT) depL
--				    in failM ("Missing labels: "++(show violatingL))
--
--dependencyLabels :: TComposition -> TContext
--dependencyLabels (TExp _ TByLetter TNotPositioned) = Set.fromList [Quantization]
--dependencyLabels (TExp _ TByNum TNotPositioned) = Set.fromList [Quantization]
--dependencyLabels (TExp _ TByLetter TPositioned ) = Set.fromList [Quantization]
--dependencyLabels (TExp _ TByNum TPositioned) = Set.fromList [Quantization]
--
--showCodeWithPos :: Composition -> String
--showCodeWithPos comp = (showCode comp)++" at: "++(show (findFirstPos comp))
--
--showCode :: Composition -> String
--showCode (NoteN note n p) = note++(show n)
--showCode (Dotted comp) = (showCode comp)++"."
--showCode (Concatted c1 c2) = (showCode c1)++(showCode c2)
--
--findFirstPos :: Composition -> Pos
--findFirstPos (NoteN n s p) = p
--findFirstPos (Dotted comp) = findFirstPos comp
--findFirstPos (Concatted c1 c2) = findFirstPos c1
--
--
--typeCheckComp :: Composition -> SEM TComposition
--typeCheckComp (NoteN _ _ _) = return $ TExp TNote TByLetter TPositioned
--
--typeCheckComp (Dotted comp) = do compT <- typeCheckComp comp
--				 assertNotParallelComp compT (showCodeWithPos comp)
--				 return $ asSerial compT
--
--typeCheckComp (Concatted c1 c2) = do c1T <- typeCheckComp c1
--				     assertNotParallelComp c1T (showCodeWithPos c1)
--				     c2T <- typeCheckComp c2
--			     	     assertNotParallelComp c2T (showCodeWithPos c2)
--				     assertEqualComps c1T c2T (showCodeWithPos (Concatted c1 c2))
--				     return $ asSerial c1T
--
--typeCheckCon :: Context -> SEM TContext
--typeCheckCon con = let labels = [Quantization]
--		       presentLabels = Prelude.filter (\label -> Map.member label con) labels
--		   in do return $ Set.fromList presentLabels
